{-# LANGUAGE OverloadedStrings #-}

-- Protocol adapted from ipgw-py-manager; see third-party/ipgw-py-manager.LICENSE.
module NEU.IPGW.Protocol (
    LoginForm (..),
    GatewayStatus (..),
    parseLoginForm,
    extractPublicKey,
    encryptCredentials,
    decodePublicKey,
    decodeReply,
    parseStatus,
    fieldText,
    resolveURL,
    queryValue,
    loginResult,
    logoutResult,
) where

import Crypto.PubKey.RSA.PKCS15 qualified as RSA
import Crypto.PubKey.RSA.Types (PublicKey (..))
import Data.Aeson (Value (..), eitherDecodeStrict')
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Char (isSpace)
import Data.List (find)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Network.HTTP.Types.URI (parseQuery)
import Network.URI (parseURI, parseURIReference, relativeTo, uriQuery)
import Text.HTML.TagSoup (Tag (..), fromAttrib, parseTags)

data LoginForm = LoginForm
    {formAction :: Text, formLT :: Text, formExecution :: Text, formScript :: Text}
    deriving (Eq, Show)

data GatewayStatus = Offline Value | Online Value deriving (Eq, Show)

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

-- Accept plain JSON or a single JSONP envelope, never arbitrary surrounding text.
decodeReply :: BS.ByteString -> Either Text Value
decodeReply bytes = either (const $ Left "网关返回了无效 JSON/JSONP") Right $ eitherDecodeStrict' payload
  where
    stripped = BS.dropWhileEnd space $ BS.dropWhile space bytes
    space = isSpace . toEnum . fromIntegral
    payload
        | BS.take 1 stripped == "{" = stripped
        | otherwise =
            let (callback, suffix) = BS.break (== 40) stripped
                inner = BS.drop 1 suffix
                ending = BS.dropWhileEnd space $ if BS.isSuffixOf ";" inner then BS.init inner else inner
             in if not (BS.null callback) && BS.all validCallback callback && BS.isSuffixOf ")" ending
                    then BS.init ending
                    else BS.empty
    validCallback c = (c >= 48 && c <= 57) || (c >= 65 && c <= 90) || (c >= 97 && c <= 122) || c == 95

fieldText :: Text -> Value -> Maybe Text
fieldText key (Object object) = case KM.lookup (K.fromText key) object of
    Just (String value) -> Just value
    Just (Number value) -> Just $ T.pack $ either show show (floatingOrInteger value :: Either Double Integer)
    _ -> Nothing
fieldText _ _ = Nothing

parseStatus :: Value -> Either Text GatewayStatus
parseStatus value = case fieldText "error" value of
    Just "ok" -> Right $ Online value
    Just "not_online_error" -> Right $ Offline value
    _ -> Left "无法识别网关状态响应"

resolveURL :: Text -> Text -> Either Text Text
resolveURL base relative = do
    b <- maybe (Left "无效基准 URL") Right $ parseURI (T.unpack base)
    r <- maybe (Left "无效相对 URL") Right $ parseURIReference (T.unpack relative)
    pure $ T.pack $ show $ r `relativeTo` b

queryValue :: BS.ByteString -> Text -> Either Text Text
queryValue key url = do
    uri <- maybe (Left "网关重定向 URL 无效") Right $ parseURI (T.unpack url)
    value <- maybe (Left $ "网关重定向缺少 " <> TE.decodeUtf8 key) Right $ lookup key (parseQuery $ TE.encodeUtf8 $ T.pack $ uriQuery uri) >>= id
    if BS.null value then Left "网关重定向参数为空" else either (const $ Left "重定向参数编码无效") Right $ TE.decodeUtf8' value

loginResult :: Value -> Either Text ()
loginResult value
    | numeric "code" value == Just 0 && fieldText "message" value == Just "success" = Right ()
    | fieldText "message" value == Just "E2616: Arrearage users." = Left "账号已欠费"
    | fieldText "message" value == Just "no_response_data_error" = Left "网关未返回账号数据；缴费后请重新连接校园网"
    | otherwise = Left "网关拒绝登录，返回了未识别的结果"

logoutResult :: Bool -> Value -> Either Text ()
logoutResult allDevices value
    | numeric (if allDevices then "code" else "ecode") value == Just 0 = Right ()
    | fieldText "error_msg" value == Just "你没有在线。" = Left "当前设备不在线"
    | allDevices && numeric "code" value == Just 1 = Left "当前账号不在线"
    | otherwise = Left "网关拒绝下线请求"

numeric :: Text -> Value -> Maybe Integer
numeric key (Object value) = case KM.lookup (K.fromText key) value of
    Just (Number n) | n == 0 -> Just 0
    Just (Number n) | n == 1 -> Just 1
    _ -> Nothing
numeric _ _ = Nothing
