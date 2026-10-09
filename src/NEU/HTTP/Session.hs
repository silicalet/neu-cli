{-# LANGUAGE OverloadedStrings #-}

module NEU.HTTP.Session (
    Session,
    Origin,
    origin,
    sameOrigin,
    RequestSpec (..),
    Reply (..),
    Transport,
    ServiceError (..),
    newSession,
    sessionWithTransport,
    request,
    RedirectRule (..),
    withRedirectRules,
) where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar (modifyMVar, newMVar)
import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import NEU.HTTP.URL (resolveURL)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hLocation, hReferer, hUserAgent, statusCode)
import Network.URI (parseURI, uriAuthority, uriFragment, uriPort, uriQuery, uriRegName, uriScheme, uriUserInfo)
import Text.Read (readMaybe)

data ServiceError = ServiceError Text deriving (Show)
instance Exception ServiceError

-- Origins include the port: cookies themselves do not isolate different ports.
data Origin = Origin String String Int deriving (Eq, Show)

origin :: Text -> Either Text Origin
origin url = do
    uri <- maybe invalid Right $ parseURI $ T.unpack url
    authority <- maybe invalid Right $ uriAuthority uri
    let scheme = uriScheme uri
        host = uriRegName authority
    unless (scheme `elem` ["http:", "https:"] && not (null host) && null (uriUserInfo authority) && null (uriFragment uri)) invalid
    port <- case uriPort authority of
        "" -> Right $ if scheme == "https:" then 443 else 80
        ':' : digits | not (null digits) && all (`elem` ['0' .. '9']) digits -> maybe invalid Right $ readMaybe digits
        _ -> invalid
    unless (port > 0 && port <= 65535) invalid
    pure $ Origin scheme (T.unpack $ T.toLower $ T.pack host) port
  where
    invalid = Left "无效的 HTTP(S) 服务地址"

sameOrigin :: Origin -> Text -> Bool
sameOrigin expected url = origin url == Right expected

data RequestSpec = RequestSpec
    { requestURL :: Text
    , requestForm :: Maybe [(BS.ByteString, BS.ByteString)]
    , requestReferer :: Maybe Text
    }
    deriving (Eq, Show)

data Reply = Reply {replyURL :: Text, replyBody :: BS.ByteString} deriving (Eq, Show)
type Transport = RequestSpec -> IO Reply
data RedirectRule = RedirectRule
    { redirectSource :: Text
    , redirectTarget :: Text
    , redirectReplacement :: Text
    }
    deriving (Eq, Show)

newtype Session = Session ([RedirectRule] -> [Origin] -> RequestSpec -> IO Reply)

{- | Explicit service-specific repair of a known reverse-proxy redirect.
Source matches the full URL except its query (which may contain a ticket).
Target matches exactly; replacement still passes the ordinary allowlist.
-}
withRedirectRules :: [RedirectRule] -> Session -> Session
withRedirectRules rules (Session send) = Session $ \_ -> send rules

-- Injected transports model a complete redirect chain; real transports check
-- every hop. In either case the initial and final destinations are checked.
sessionWithTransport :: Transport -> Session
sessionWithTransport send = Session $ \_ _ -> send

request :: Session -> [Origin] -> RequestSpec -> IO Reply
request (Session send) allowed spec = do
    checkDestination allowed $ requestURL spec
    response <- send [] allowed spec
    checkDestination allowed $ replyURL response
    pure response

checkDestination :: [Origin] -> Text -> IO ()
checkDestination allowed url =
    unless (any (`sameOrigin` url) allowed) $
        throwIO $
            ServiceError "请求或重定向指向了服务允许范围之外的地址"

-- In-memory only. Use a separate session for a different identity, or for
-- services whose same-host cookies conflict. Each redirect chain is serialized.
newSession :: Bool -> Int -> IO Session
newSession bypass timeoutSeconds = do
    unless (timeoutSeconds >= 1 && timeoutSeconds <= 300) $
        throwIO $
            ServiceError "请求超时必须在 1–300 秒之间"
    let settings = if bypass then HTTP.managerSetProxy HTTP.proxyFromRequest tlsManagerSettings else tlsManagerSettings
    manager <- HTTP.newManager settings
    cookies <- newMVar $ HTTP.createCookieJar []
    let send rules allowed spec = modifyMVar cookies (\jar -> loop rules allowed (0 :: Int) jar spec) `catch` networkError
        loop rules allowed count jar spec = do
            when (count > 10) $ throwIO $ ServiceError "校园服务重定向次数过多"
            checkDestination allowed $ requestURL spec
            parsed <- HTTP.parseRequest $ T.unpack $ requestURL spec
            let headers =
                    [(hUserAgent, "neu/0.1"), ("Accept-Language", "zh-CN"), ("X-Requested-With", "XMLHttpRequest")]
                        ++ maybe [] (\r -> [(hReferer, TE.encodeUtf8 r)]) (requestReferer spec)
                base =
                    parsed
                        { HTTP.cookieJar = Just jar
                        , HTTP.redirectCount = 0
                        , HTTP.responseTimeout = HTTP.responseTimeoutMicro (timeoutSeconds * 1000000)
                        , HTTP.requestHeaders = headers
                        , HTTP.checkResponse = \_ _ -> pure ()
                        }
                req = maybe base (`HTTP.urlEncodedBody` base) $ requestForm spec
            response <- HTTP.httpLbs req manager
            let status = statusCode $ HTTP.responseStatus response
                nextJar = HTTP.responseCookieJar response
            if status `elem` [301, 302, 303, 307, 308]
                then do
                    -- Yii uses X-Redirect for XMLHttpRequest responses.
                    -- Apply exactly the same destination checks to both headers.
                    let responseHeaders = HTTP.responseHeaders response
                    location <-
                        maybe (throwIO $ ServiceError "重定向缺少 Location 或 X-Redirect") pure $
                            lookup hLocation responseHeaders <|> lookup "X-Redirect" responseHeaders
                    next <- either (throwIO . ServiceError) pure $ resolveURL (requestURL spec) (TE.decodeUtf8With TE.lenientDecode location)
                    when (requestForm spec /= Nothing && status `elem` [307, 308]) $
                        throwIO $
                            ServiceError "拒绝重定向重发认证表单"
                    let repaired = repairRedirect rules (requestURL spec) next
                    loop rules allowed (count + 1) nextJar (RequestSpec repaired Nothing Nothing)
                else do
                    unless (status >= 200 && status < 300) $
                        throwIO $
                            ServiceError $
                                "校园服务 HTTP 错误: " <> T.pack (show status)
                    pure (nextJar, Reply (requestURL spec) (BL.toStrict $ HTTP.responseBody response))
    pure $ Session send
  where
    networkError :: HTTP.HttpException -> IO a
    networkError _ = throwIO $ ServiceError "校园服务请求失败（网络、超时或 TLS 错误）；请检查校园网连接和代理设置"

repairRedirect :: [RedirectRule] -> Text -> Text -> Text
repairRedirect rules source target = case [redirectReplacement rule | rule <- rules, withoutQuery source == Just (redirectSource rule), target == redirectTarget rule] of
    replacement : _ -> replacement
    [] -> target
  where
    withoutQuery url = (\uri -> T.pack $ show uri {uriQuery = ""}) <$> parseURI (T.unpack url)
