{-# LANGUAGE OverloadedStrings #-}

module NEU.HTTP.URL (resolveURL, queryValue, urlWith) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.URI (parseQuery, renderQuery)
import Network.URI (parseURI, parseURIReference, relativeTo, uriQuery)

urlWith :: Text -> [(BS.ByteString, BS.ByteString)] -> Text
urlWith url pairs = url <> TE.decodeUtf8 (renderQuery True $ map (\(k, v) -> (k, Just v)) pairs)

resolveURL :: Text -> Text -> Either Text Text
resolveURL base relative = do
    b <- maybe (Left "无效基准 URL") Right $ parseURI (T.unpack base)
    r <- maybe (Left "无效相对 URL") Right $ parseURIReference (T.unpack relative)
    pure $ T.pack $ show $ r `relativeTo` b

queryValue :: BS.ByteString -> Text -> Either Text Text
queryValue key url = do
    uri <- maybe (Left "重定向 URL 无效") Right $ parseURI (T.unpack url)
    value <- maybe (Left $ "重定向缺少 " <> TE.decodeUtf8 key) Right $ lookup key (parseQuery $ TE.encodeUtf8 $ T.pack $ uriQuery uri) >>= id
    if BS.null value then Left "重定向参数为空" else either (const $ Left "重定向参数编码无效") Right $ TE.decodeUtf8' value
