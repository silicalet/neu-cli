{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Client (
    Client,
    RequestSpec (..),
    Reply (..),
    Transport,
    newClient,
    clientWithTransport,
    clientWithSession,
    getStatus,
    login,
    logout,
    IPGWError (..),
) where

import Control.Exception (Exception, catch, throwIO)
import Data.Aeson (Value)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock.POSIX (getPOSIXTime)
import NEU.Auth.CAS qualified as CAS
import NEU.HTTP.Session
import NEU.HTTP.URL (urlWith)
import NEU.IPGW.Protocol
import Network.URI (parseURI, uriPath)

data IPGWError = IPGWError Text deriving (Show)
instance Exception IPGWError

newtype Client = Client Session

-- | Share an in-memory CAS session with other services for the same identity.
clientWithSession :: Session -> Client
clientWithSession = Client

clientWithTransport :: Transport -> Client
clientWithTransport = Client . sessionWithTransport

newClient :: Bool -> Int -> IO Client
newClient bypass seconds = gatewayErrors $ Client <$> newSession bypass seconds

gatewayErrors :: IO a -> IO a
gatewayErrors action =
    action
        `catch` (\(ServiceError message) -> throwIO $ IPGWError message)
        `catch` (\err -> throwIO $ IPGWError $ CAS.casErrorMessage err)

gatewayService :: Text -> IO CAS.Service
gatewayService url = checked $ CAS.mkService url [portal, "https://ipgw.neu.edu.cn"]

sendGateway :: Client -> RequestSpec -> IO Reply
sendGateway (Client session) spec = gatewayErrors $ do
    service <- gatewayService portal
    CAS.requestService session service spec

portal :: Text
portal = "http://ipgw.neu.edu.cn"
get :: Client -> Text -> IO Reply
get client url = sendGateway client $ RequestSpec url Nothing Nothing

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
login client@(Client session) username password = gatewayErrors $ do
    ac <- acid client
    service <- gatewayService $ urlWith (portal <> "/srun_portal_sso") [("ac_id", TE.encodeUtf8 ac)]
    response <- CAS.authenticate session service username password
    let path = uriPath <$> parseURI (T.unpack $ replyURL response)
    callbackAC <- checked $ queryValue "ac_id" $ replyURL response
    if path /= Just "/srun_portal_sso" || callbackAC /= ac then throwIO $ IPGWError "无法识别网关认证回调" else pure ()
    ticket <- checked $ queryValue "ticket" $ replyURL response
    let params = [("ac_id", TE.encodeUtf8 ac), ("ticket", TE.encodeUtf8 ticket)]
    result <-
        sendGateway client $
            RequestSpec
                (urlWith (portal <> "/v1/srun_portal_sso") params)
                Nothing
                (Just $ urlWith (portal <> "/srun_portal_sso") params)
    json result >>= checked . loginResult

logout :: Client -> Bool -> GatewayStatus -> IO ()
logout _ _ (Offline _) = throwIO $ IPGWError "当前设备不在线"
logout client allDevices (Online info) = do
    ac <- acid client
    result <-
        if allDevices
            then sendGateway client $ RequestSpec (portal <> "/v1/batch-online-drop") (Just []) Nothing
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
