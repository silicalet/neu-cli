{-# LANGUAGE OverloadedStrings #-}

-- Hostname encryption follows the WebVPN front end used by webvpn.neu.edu.cn:
-- AES-128-CFB with the institution key as both key and IV. Only the hostname
-- ciphertext is kept; the path and query stay in the clear.
module NEU.WebVPN (
    WebVPNURL (..),
    encodeWebVPN,
    portal,
) where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher (..), IV, cipherInit, makeIV)
import Crypto.Error (CryptoFailable (..))
import Data.ByteArray (convert)
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.URI (URI (..), URIAuth (..), parseURI, uriToString)

data WebVPNURL = WebVPNURL {sourceURL :: Text, webvpnURL :: Text} deriving (Eq, Show)

portal :: Text
portal = "https://webvpn.neu.edu.cn"

-- The published institution key. Its ASCII hex is the fixed URL prefix.
keyMaterial :: BS.ByteString
keyMaterial = "b0A58a69394ce73@"

encodeWebVPN :: Text -> Either Text WebVPNURL
encodeWebVPN raw = do
    let stripped = T.strip raw
    whenBlank stripped
    uri <- maybe (Left "URL 无效") Right $ parseURI (T.unpack $ ensureScheme stripped)
    auth <- maybe (Left "URL 缺少主机名") Right $ uriAuthority uri
    scheme <- schemeName (uriScheme uri)
    host <- hostname (T.pack $ uriRegName auth)
    port <- explicitPort scheme (uriPort auth)
    whenBlank host
    encrypted <- encryptHost host
    let service = case port of
            Nothing -> scheme
            Just number -> scheme <> "-" <> number
        target =
            portal
                <> "/"
                <> service
                <> "/"
                <> encrypted
                <> T.pack (uriPath uri)
                <> T.pack (uriQuery uri)
    pure $ WebVPNURL (T.pack $ uriToString id uri "") target
  where
    whenBlank value = if T.null value then Left "URL 不能为空" else Right ()
    ensureScheme value
        | "://" `T.isInfixOf` value = value
        | otherwise = "http://" <> value
    schemeName "http:" = Right "http"
    schemeName "https:" = Right "https"
    schemeName _ = Left "只支持 http 和 https"
    -- Brackets are URI syntax. network-uri also strips them, so an address
    -- that still contains a colon here is an IPv6 literal, which this gateway
    -- encoding does not define.
    hostname name
        | T.null name = Left "URL 缺少主机名"
        | ":" `T.isInfixOf` name || "[" `T.isInfixOf` name || "]" `T.isInfixOf` name = Left "不支持 IPv6 字面量"
        | otherwise = Right name
    explicitPort scheme (':' : digits)
        | not (null digits) && all isDigit digits =
            if (scheme == "http" && digits == "80") || (scheme == "https" && digits == "443")
                then Right Nothing
                else Right $ Just (T.pack digits)
    explicitPort _ "" = Right Nothing
    explicitPort _ _ = Left "端口无效"

encryptHost :: Text -> Either Text Text
encryptHost host = do
    let plain = TE.encodeUtf8 host
        block = 16
        padded = plain <> BS.replicate (block - (BS.length plain `mod` block)) 0
    cipher <- case cipherInit keyMaterial :: CryptoFailable AES128 of
        CryptoPassed value -> Right value
        CryptoFailed _ -> Left "WebVPN 密钥无效"
    iv <- maybe (Left "WebVPN IV 无效") Right (makeIV keyMaterial :: Maybe (IV AES128))
    let ciphertext = BS.take (BS.length plain) $ convert (cfbEncrypt cipher iv padded)
    pure $ TE.decodeUtf8 (toHex keyMaterial <> toHex ciphertext)

toHex :: BS.ByteString -> BS.ByteString
toHex = BS.concatMap $ \byte ->
    BS.pack [digit (byte `div` 16), digit (byte `mod` 16)]
  where
    digit n = if n < 10 then 48 + n else 87 + n
