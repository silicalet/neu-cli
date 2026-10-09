{-# LANGUAGE OverloadedStrings #-}

-- Protocol adapted from ipgw-py-manager; see third-party/ipgw-py-manager.LICENSE.
module NEU.IPGW.Protocol (
    GatewayStatus (..),
    decodeReply,
    parseStatus,
    fieldText,
    resolveURL,
    queryValue,
    loginResult,
    logoutResult,
) where

import Data.Aeson (Value (..), eitherDecodeStrict')
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Char (isSpace)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import NEU.HTTP.URL (queryValue, resolveURL)

data GatewayStatus = Offline Value | Online Value deriving (Eq, Show)

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
