{-# LANGUAGE OverloadedStrings #-}

-- Choose a direct campus connection when the gateway reports this device
-- online; otherwise authenticate to WebVPN and rewrite library URLs.
module NEU.Library.Access (
    LibraryError (..),
    Route (..),
    routeName,
    openLibrary,
    fetchCatalog,
    fetchNews,
) where

import Control.Exception (Exception, catch, throwIO)
import Data.Aeson (object)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import NEU.Auth.CAS qualified as CAS
import NEU.HTTP.Session
import NEU.HTTP.URL (resolveURL)
import NEU.IPGW.Client (IPGWError (..), getStatus)
import NEU.IPGW.Client qualified as IPGW
import NEU.IPGW.Protocol (GatewayStatus (..))
import NEU.Library
import NEU.WebVPN (encodeWebVPN, webvpnURL)
import Text.HTML.TagSoup (Tag (..), parseTags)

data LibraryError = LibraryError Text deriving (Show)
instance Exception LibraryError

data Route = Direct | WebVPN deriving (Eq, Show)

routeName :: Route -> Text
routeName Direct = "direct"
routeName WebVPN = "webvpn"

data Connection = Connection Route Session [Origin] Text Text

catalogBase :: Text
catalogBase = "http://210.30.202.58:8991"
libraryBase :: Text
libraryBase = "http://www.lib.neu.edu.cn"
webvpnLogin :: Text
webvpnLogin = "https://webvpn.neu.edu.cn/login?cas_login=true"

openLibrary :: Bool -> Int -> Text -> Text -> IO (Route, Connection)
openLibrary bypass timeout username password = do
    selected <- detectRoute bypass timeout
    session <- newSession bypass timeout
    connection <- case selected of
        Direct -> pure $ Connection Direct session (mustOrigins [catalogBase, libraryBase]) catalogBase libraryBase
        WebVPN -> do
            service <- checked $ CAS.mkService webvpnLogin ["https://webvpn.neu.edu.cn"]
            _ <- libraryErrors $ CAS.authenticate session service username password
            catalog <- rewrite catalogBase
            library <- rewrite libraryBase
            pure $ Connection WebVPN session (mustOrigins ["https://webvpn.neu.edu.cn"]) catalog library
    pure (selected, connection)
  where
    rewrite url = checked $ webvpnURL <$> encodeWebVPN url

detectRoute :: Bool -> Int -> IO Route
detectRoute bypass timeout = do
    client <- IPGW.newClient bypass timeout
    status <- getStatus client `catch` (\(_ :: IPGWError) -> pure (Offline (object [])))
    pure $ case status of Online _ -> Direct; Offline _ -> WebVPN

fetchCatalog :: Connection -> CatalogQuery -> IO [CatalogHit]
fetchCatalog connection query = do
    first <- get connection $ catalogRequest (catalogOf connection) query
    page <- case formAction (TE.decodeUtf8With lenientDecode $ replyBody first) of
        Nothing -> pure first
        Just relative -> do
            url <- checked $ resolveURL (replyURL first) relative
            get connection $ catalogRequest (T.takeWhile (/= '?') url) query
    checked $ parseCatalog (queryLimit query) (replyBody page)

fetchNews :: Connection -> Text -> IO [NewsItem]
fetchNews connection kind = do
    let spec = newsRequest (libraryOf connection) kind
    reply <- get connection spec
    checked $ parseNews (replyURL reply) (replyBody reply)

catalogOf :: Connection -> Text
catalogOf (Connection _ _ _ catalog _) = catalog
libraryOf :: Connection -> Text
libraryOf (Connection _ _ _ _ library) = library

get :: Connection -> RequestSpec -> IO Reply
get (Connection _ session allowed _ _) spec = libraryErrors $ request session allowed spec

formAction :: Text -> Maybe Text
formAction html = case [action | TagOpen "form" attrs <- parseTags html, Just action <- [lookup "action" attrs], not (T.null action)] of
    action : _ -> Just action
    [] -> Nothing

mustOrigins :: [Text] -> [Origin]
mustOrigins = either (error . T.unpack) id . traverse origin

checked :: Either Text a -> IO a
checked = either (throwIO . LibraryError) pure

libraryErrors :: IO a -> IO a
libraryErrors action =
    action
        `catch` (\(ServiceError message) -> throwIO $ LibraryError message)
        `catch` (\err -> throwIO $ LibraryError $ CAS.casErrorMessage err)
