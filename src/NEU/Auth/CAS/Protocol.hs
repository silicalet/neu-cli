{-# LANGUAGE OverloadedStrings #-}

-- Protocol adapted from ipgw-py-manager; see third-party/ipgw-py-manager.LICENSE.
module NEU.Auth.CAS.Protocol (
    LoginForm (..),
    parseLoginForm,
    extractPublicKey,
    encryptCredentials,
    decodePublicKey,
) where

import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.PubKey.RSA.Types (PublicKey (..))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.HTML.TagSoup (Tag (..), fromAttrib, parseTags)

data LoginForm = LoginForm
    {formAction :: Text, formLT :: Text, formExecution :: Text, formScript :: Text}
    deriving (Eq, Show)

parseLoginForm :: Text -> Either Text LoginForm
parseLoginForm html = do
    let tags = parseTags html
    form <-
        maybe (Left "SSO 页面缺少 loginForm") Right $
            find (\t -> isTag "form" t && fromAttrib "id" t == "loginForm") tags
    let inside = takeWhile (/= TagClose "form") (drop 1 $ dropWhile (/= form) tags)
        input key value =
            maybe (Left $ "SSO 表单缺少 " <> value) (required "value") $
                find (\t -> isTag "input" t && fromAttrib key t == value) inside
    action <- required "action" form
    lt <- input "id" "lt"
    execution <- input "name" "execution"
    script <-
        maybe (Left "SSO 页面缺少 login_neu.js") (required "src") $
            find (\t -> isTag "script" t && "login_neu.js" `T.isInfixOf` fromAttrib "src" t) tags
    pure $ LoginForm action lt execution script
  where
    isTag name (TagOpen n _) = name == n
    isTag _ _ = False
    required key tag =
        let value = fromAttrib key tag
         in if T.null value then Left ("SSO 表单属性为空: " <> key) else Right value

extractPublicKey :: Text -> Either Text Text
extractPublicKey script = do
    let (_, suffix) = T.breakOn "publicKeyStr" script
        assignment = T.stripStart (T.drop (T.length "publicKeyStr") suffix)
    rest <- maybe (Left "SSO 脚本缺少 publicKeyStr 赋值") Right (T.stripPrefix "=" assignment)
    case T.uncons (T.stripStart rest) of
        Just (quote, value)
            | quote == '"' || quote == '\'' ->
                let (key, end) = T.break (== quote) value
                 in if T.null key || T.null end then Left "SSO RSA 公钥为空或不完整" else Right key
        _ -> Left "无法解析 SSO RSA 公钥"

-- Strict DER parsing of the SPKI public key used by the login page.
decodePublicKey :: Text -> Either Text PublicKey
decodePublicKey encoded = do
    der <- either (const $ Left "RSA 公钥不是有效 Base64") Right (B64.decode $ TE.encodeUtf8 encoded)
    (outer, rest) <- tlv 0x30 der
    ensure (BS.null rest) "RSA 公钥有多余数据"
    (algorithm, keyPart) <- tlv 0x30 outer
    ensure (algorithm == BS.pack [6, 9, 42, 134, 72, 134, 247, 13, 1, 1, 1, 5, 0]) "RSA 公钥算法不受支持"
    (bits, end) <- tlv 3 keyPart
    ensure (BS.null end && not (BS.null bits) && BS.head bits == 0) "RSA BIT STRING 无效"
    (key, trailing) <- tlv 0x30 (BS.drop 1 bits)
    ensure (BS.null trailing) "RSA 公钥结构无效"
    (nBytes, ePart) <- tlv 2 key
    (eBytes, leftover) <- tlv 2 ePart
    ensure (BS.null leftover && not (BS.null nBytes) && not (BS.null eBytes)) "RSA 参数缺失"
    let n = integer nBytes
        e = integer eBytes
        size = BS.length (BS.dropWhile (== 0) nBytes)
    ensure (size >= 128 && size <= 1024 && odd n && e >= 3 && odd e) "RSA 参数无效"
    pure $ PublicKey size n e
  where
    integer = BS.foldl' (\n b -> n * 256 + fromIntegral b) 0
    tlv tag bytes = do
        ensure (BS.length bytes >= 2 && BS.head bytes == tag) "RSA DER 标签无效"
        let first = BS.index bytes 1
            count = fromIntegral (first - 128)
        (len, value) <-
            if first < 128
                then Right (fromIntegral first, BS.drop 2 bytes)
                else do
                    ensure (count > 0 && count <= 4 && BS.length bytes >= 2 + count) "RSA DER 长度无效"
                    let len = integer (BS.take count $ BS.drop 2 bytes)
                    ensure (len <= fromIntegral (BS.length bytes)) "RSA DER 数据截断"
                    Right (fromInteger len, BS.drop (2 + count) bytes)
        ensure (BS.length value >= len) "RSA DER 数据截断"
        pure (BS.take len value, BS.drop len value)
    ensure True _ = Right ()
    ensure False message = Left message

encryptCredentials :: Text -> Text -> Text -> IO (Either Text Text)
encryptCredentials username password encoded = case decodePublicKey encoded of
    Left err -> pure $ Left err
    Right key -> do
        result <- RSA.encrypt key (TE.encodeUtf8 $ username <> password)
        pure $ either (const $ Left "凭据过长或 RSA 加密失败") (Right . TE.decodeUtf8 . B64.encode) result
