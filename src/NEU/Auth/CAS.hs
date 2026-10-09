{-# LANGUAGE OverloadedStrings #-}

-- | NEU CAS authentication, independent of gateway activation or account files.
module NEU.Auth.CAS (
    Service,
    mkService,
    serviceURL,
    CASError (..),
    casErrorMessage,
    authenticate,
    requestService,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TE
import NEU.Auth.CAS.Protocol
import NEU.HTTP.Session
import NEU.HTTP.URL (resolveURL, urlWith)
import Text.HTML.TagSoup (Tag (..), innerText, parseTags)

data Service = Service Text [Origin] Origin

serviceURL :: Service -> Text
serviceURL (Service url _ _) = url

{- | Explicit HTTP(S) origins allowed for this service's callback and landing
pages. No wildcard domains or ports; the CAS origin is added separately.
-}
mkService :: Text -> [Text] -> Either Text Service
mkService url destinations = do
    target <- origin url
    allowed <- traverse origin destinations
    cas <- origin casBase
    unless (target `elem` allowed && cas `notElem` allowed) $
        Left "服务回调必须属于业务白名单，且业务白名单不能包含 CAS"
    pure $ Service url allowed cas

data CASError
    = CredentialsRejectedOrInteractionRequired
    | InteractionRequired
    | CASProtocolError Text
    deriving (Eq, Show)
instance Exception CASError

casErrorMessage :: CASError -> Text
casErrorMessage CredentialsRejectedOrInteractionRequired = "用户名或密码错误，或认证要求额外验证"
casErrorMessage InteractionRequired = "认证需要交互；请在浏览器中完成验证"
casErrorMessage (CASProtocolError message) = message

casBase :: Text
casBase = "https://pass.neu.edu.cn"

{- | Reuse the session established by 'authenticate' for business requests.
A redirect back to CAS is deliberately rejected: authenticate explicitly.
-}
requestService :: Session -> Service -> RequestSpec -> IO Reply
requestService session (Service _ allowed _) = request session allowed

{- | Return the service landing page, including when a CAS cookie avoids the
login form. The caller MUST validate its own callback / business response:
reaching a service origin alone does not prove a successful business login.
Reuse one session sequentially for one identity. Never log credential fields.
-}
authenticate :: Session -> Service -> Text -> Text -> IO Reply
authenticate session service@(Service _ allowed cas) username password = do
    page <- get authURL
    if isLanding page
        then pure page
        else do
            form <- checked $ parseLoginForm $ bodyText page
            action <- checked $ resolveURL (replyURL page) (formAction form)
            script <- checked $ resolveURL (replyURL page) (formScript form)
            unless (sameOrigin cas action && sameOrigin cas script) $
                throwIO $
                    CASProtocolError "SSO 表单或脚本指向了不可信地址"
            key <- request session [cas] (RequestSpec script Nothing Nothing) >>= checked . extractPublicKey . bodyText
            encrypted <- encryptCredentials username password key >>= checked
            let fields =
                    map
                        (\(k, v) -> (k, TE.encodeUtf8 v))
                        [ ("rsa", encrypted)
                        , ("ul", jsLength username)
                        , ("pl", jsLength password)
                        , ("lt", formLT form)
                        , ("execution", formExecution form)
                        , ("_eventId", "submit")
                        ]
            response <- send $ RequestSpec action (Just fields) Nothing
            if isLanding response
                then pure response
                else case pageTitle response of
                    "智慧东大--统一身份认证" -> throwIO CredentialsRejectedOrInteractionRequired
                    -- Existing NEU email-binding detour: retry the service once,
                    -- without resubmitting credentials or looping indefinitely.
                    "智慧东大" -> do
                        retried <- get authURL
                        if isLanding retried then pure retried else throwIO InteractionRequired
                    _ -> throwIO InteractionRequired
  where
    authURL = urlWith (casBase <> "/tpass/login") [("service", TE.encodeUtf8 $ serviceURL service)]
    send = request session (cas : allowed)
    get url = send $ RequestSpec url Nothing Nothing
    isLanding reply = any (`sameOrigin` replyURL reply) allowed
    checked :: Either Text a -> IO a
    checked = either (throwIO . CASProtocolError) pure
    bodyText = TE.decodeUtf8With TE.lenientDecode . replyBody
    pageTitle =
        T.strip
            . innerText
            . takeWhile (/= TagClose "title")
            . drop 1
            . dropWhile (\t -> case t of TagOpen "title" _ -> False; _ -> True)
            . parseTags
            . bodyText
    -- Browser JavaScript string lengths are UTF-16 code units, not characters.
    jsLength = T.pack . show . T.foldl' (\n c -> n + if fromEnum c > 0xffff then 2 else 1) (0 :: Int)
