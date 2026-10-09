{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Client (
    Client,
    RequestSpec (..),
    Reply (..),
    Transport,
    newClient,
    clientWithTransport,
    getStatus,
    login,
    logout,
    IPGWError (..),
) where

import Control.Exception (Exception, catch, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (Value)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import NEU.IPGW.Protocol
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hLocation, hReferer, hUserAgent, statusCode)
import Network.HTTP.Types.URI (renderQuery)
import Network.URI (parseURI, uriAuthority, uriPort, uriRegName, uriScheme, uriUserInfo)
import Text.HTML.TagSoup (Tag (..), innerText, parseTags)

data IPGWError = IPGWError Text deriving (Show)
instance Exception IPGWError

data RequestSpec = RequestSpec
    { requestURL :: Text
    , requestForm :: Maybe [(BS.ByteString, BS.ByteString)]
    , requestReferer :: Maybe Text
    }
    deriving (Eq, Show)

data Reply = Reply {replyURL :: Text, replyBody :: BS.ByteString} deriving (Eq, Show)
type Transport = RequestSpec -> IO Reply
newtype Client = Client Transport

clientWithTransport :: Transport -> Client
clientWithTransport = Client

-- Cookie jar is shared across CAS, redirects and portal operations.
newClient :: Bool -> Int -> IO Client
newClient bypass timeoutSeconds = do
    let settings =
            if bypass
                then HTTP.managerSetProxy (HTTP.proxyFromRequest) tlsManagerSettings
                else tlsManagerSettings
    manager <- HTTP.newManager settings
    cookies <- newIORef $ HTTP.createCookieJar []
    let send spec = loop (0 :: Int) spec `catch` networkError
        loop count spec = do
            when (count > 10) $ throwIO $ IPGWError "网关重定向次数过多"
            jar <- readIORef cookies
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
            writeIORef cookies $ HTTP.responseCookieJar response
            let status = statusCode $ HTTP.responseStatus response
            if status `elem` [301, 302, 303, 307, 308]
                then do
                    location <- need "重定向缺少 Location" $ lookup hLocation (HTTP.responseHeaders response)
                    next <- checked $ resolveURL (requestURL spec) (TE.decodeUtf8With TE.lenientDecode location)
                    -- Never forward credentials to a redirect target.
                    when (requestForm spec /= Nothing && status `elem` [307, 308]) $
                        throwIO $
                            IPGWError "拒绝重定向重发认证表单"
                    unless (allowedRedirect next) $ throwIO $ IPGWError "认证重定向到了非校园认证地址"
                    loop (count + 1) (RequestSpec next Nothing Nothing)
                else do
                    unless (status >= 200 && status < 300) $ throwIO $ IPGWError ("校园服务 HTTP 错误: " <> T.pack (show status))
                    pure $ Reply (requestURL spec) (BL.toStrict $ HTTP.responseBody response)
    pure $ Client send
  where
    networkError :: HTTP.HttpException -> IO a
    networkError _ = throwIO $ IPGWError "校园服务请求失败（网络、超时或 TLS 错误）；请检查校园网连接和代理设置"

allowedRedirect :: Text -> Bool
allowedRedirect url = case parseURI $ T.unpack url of
    Just uri -> case uriAuthority uri of
        Just authority ->
            null (uriUserInfo authority)
                && null (uriPort authority)
                && ( (uriRegName authority == "pass.neu.edu.cn" && uriScheme uri == "https:")
                        || (uriRegName authority == "ipgw.neu.edu.cn" && uriScheme uri `elem` ["http:", "https:"])
                   )
        Nothing -> False
    Nothing -> False

trustedSSO :: Text -> Bool
trustedSSO url = case parseURI $ T.unpack url of
    Just uri ->
        uriScheme uri == "https:" && case uriAuthority uri of
            Just a -> uriRegName a == "pass.neu.edu.cn" && null (uriUserInfo a) && null (uriPort a)
            Nothing -> False
    Nothing -> False

portal :: Text
portal = "http://ipgw.neu.edu.cn"
sso :: Text
sso = "https://pass.neu.edu.cn"

urlWith :: Text -> [(BS.ByteString, BS.ByteString)] -> Text
urlWith url pairs = url <> TE.decodeUtf8 (renderQuery True $ map (\(k, v) -> (k, Just v)) pairs)

get :: Client -> Text -> IO Reply
get (Client send) url = send $ RequestSpec url Nothing Nothing

checked :: Either Text a -> IO a
checked = either (throwIO . IPGWError) pure
need :: Text -> Maybe a -> IO a
need message = maybe (throwIO $ IPGWError message) pure
json :: Reply -> IO Value
json = checked . decodeReply . replyBody

getStatus :: Client -> IO GatewayStatus
getStatus client = do
    time <- T.pack . show . (floor :: Double -> Integer) . (* 1000) . realToFrac <$> getPOSIXTime
    let timestamp = TE.encodeUtf8 time
    get client (urlWith (portal <> "/cgi-bin/rad_user_info") [("callback", "neu_" <> timestamp), ("_", timestamp)]) >>= json >>= checked . parseStatus

acid :: Client -> IO Text
acid client = get client portal >>= checked . queryValue "ac_id" . replyURL

login :: Client -> Text -> Text -> IO ()
login client@(Client send) username password = do
    ac <- acid client
    let authURL = urlWith (sso <> "/tpass/login") [("service", TE.encodeUtf8 $ urlWith (portal <> "/srun_portal_sso") [("ac_id", TE.encodeUtf8 ac)])]
    page <- get client authURL
    form <- checked $ parseLoginForm (TE.decodeUtf8With TE.lenientDecode $ replyBody page)
    action <- checked $ resolveURL (replyURL page) (formAction form)
    script <- checked $ resolveURL (replyURL page) (formScript form)
    unless (trustedSSO action && trustedSSO script) $ throwIO $ IPGWError "SSO 表单或脚本指向了不可信地址"
    key <- get client script >>= checked . extractPublicKey . TE.decodeUtf8With TE.lenientDecode . replyBody
    encrypted <- encryptCredentials username password key >>= checked
    let fields =
            map
                (\(k, v) -> (k, TE.encodeUtf8 v))
                [ ("rsa", encrypted)
                , ("ul", T.pack $ show $ length $ T.unpack username)
                , ("pl", T.pack $ show $ length $ T.unpack password)
                , ("lt", formLT form)
                , ("execution", formExecution form)
                , ("_eventId", "submit")
                ]
    response <- send $ RequestSpec action (Just fields) Nothing
    ticket <- case pageTitle $ replyBody response of
        "IP控制网关" -> checked $ queryValue "ticket" (replyURL response)
        "智慧东大--统一身份认证" -> throwIO $ IPGWError "用户名或密码错误，或认证要求额外验证"
        "智慧东大" -> get client authURL >>= checked . queryValue "ticket" . replyURL
        _ -> throwIO $ IPGWError "无法识别 SSO 响应；可能需要在浏览器中完成验证"
    let params = [("ac_id", TE.encodeUtf8 ac), ("ticket", TE.encodeUtf8 ticket)]
    result <-
        send $
            RequestSpec
                (urlWith (portal <> "/v1/srun_portal_sso") params)
                Nothing
                (Just $ urlWith (portal <> "/srun_portal_sso") params)
    json result >>= checked . loginResult

pageTitle :: BS.ByteString -> Text
pageTitle body =
    T.strip $
        innerText $
            takeWhile (/= TagClose "title") $
                drop 1 $
                    dropWhile (\t -> case t of TagOpen "title" _ -> False; _ -> True) $
                        parseTags (TE.decodeUtf8With TE.lenientDecode body)

logout :: Client -> Bool -> GatewayStatus -> IO ()
logout _ _ (Offline _) = throwIO $ IPGWError "当前设备不在线"
logout client@(Client send) allDevices (Online info) = do
    ac <- acid client
    result <-
        if allDevices
            then send $ RequestSpec (portal <> "/v1/batch-online-drop") (Just []) Nothing
            else do
                username <- need "网关状态缺少用户名" $ fieldText "user_name" info
                ip <- need "网关状态缺少在线 IP" $ fieldText "online_ip" info
                get client $
                    urlWith
                        (portal <> "/cgi-bin/srun_portal")
                        [ ("callback", "neu_logout")
                        , ("action", "logout")
                        , ("username", TE.encodeUtf8 username)
                        , ("ip", TE.encodeUtf8 ip)
                        , ("ac_id", TE.encodeUtf8 ac)
                        ]
    json result >>= checked . logoutResult allDevices
