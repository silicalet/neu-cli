{-# LANGUAGE OverloadedStrings #-}

module OverviewTests (overviewTests) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.Char (isControl)
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import NEU.IPGW.Overview

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

field :: Text -> Value -> Value
field key (Object fields) = maybe Null id $ KM.lookup (Key.fromText key) fields
field _ _ = Null

hasNull :: Text -> Value -> Bool
hasNull key (Object fields) = KM.lookup (Key.fromText key) fields == Just Null
hasNull _ _ = False

array :: Value -> [Value]
array (Array values) = toList values
array _ = []

parse :: Text -> Either Text Overview
parse = parseOverview . TE.encodeUtf8

overviewTests :: IO ()
overviewTests = do
    normalHTML <- T.readFile "test/fixtures/overview.html"
    emptyHTML <- T.readFile "test/fixtures/overview-empty.html"
    multipleHTML <- T.readFile "test/fixtures/overview-multiple.html"
    normal <- right $ parse normalHTML
    empty <- right $ parse emptyHTML
    multiple <- right $ parse multipleHTML
    assert "account list-item labels, nested layout and entities decoded" $
        overviewAccount normal == AccountOverview "fixture-user" (Just "测试 & 用户") "正常"
    assert "single IPv4 device and nested policy parsed" $ case overviewDevices normal of
        [device] -> deviceIP device == "192.0.2.1" && deviceMAC device == Just "02:00:00:00:00:01" && deviceBillingPolicy device == Just "标准 网络 & 服务"
        _ -> False
    assert "exact four-place scientific amounts and formatted counters" $ case overviewProducts normal of
        [plan] ->
            productName plan == "基础 & 套餐"
                && productCost plan == Just 0.1234
                && productBalance plan == Just 10.9876
                && productTraffic plan == Just (ObservedCounter "1.23G" Nothing)
                && productDuration plan == Just (ObservedCounter "1小时2分" Nothing)
                && productDetails plan == Nothing
        _ -> False
    let normalInfo = infoJSON normal
        normalDevices = devicesJSON normal
    assert "info JSON top-level contract" $
        keys normalInfo == ["account", "online_device_count", "products", "timezone"]
            && field "online_device_count" normalInfo == Number 1
            && field "timezone" normalInfo == String "Asia/Shanghai"
            && field "account" normalInfo == object ["username" .= ("fixture-user" :: Text), "display_name" .= ("测试 & 用户" :: Text), "status" .= ("正常" :: Text)]
    assert "device JSON includes canonical local time, no invented IDs" $ case array $ field "devices" normalDevices of
        [device] ->
            keys device == ["billing_policy", "connected_at", "ip", "mac"]
                && field "connected_at" device == String "2026-10-01 09:10:11"
                && field "ip" device == String "192.0.2.1"
        _ -> False
    assert "devices JSON top-level contract" $
        keys normalDevices == ["count", "devices", "timezone"]
            && field "count" normalDevices == Number 1
            && field "timezone" normalDevices == String "Asia/Shanghai"
    assert "product JSON exposes unknown raw counters explicitly" $ case array $ field "products" normalInfo of
        [plan] ->
            keys plan == ["balance_display", "balance_yuan", "billing_policy", "cost_display", "cost_yuan", "details_text", "id", "name", "used_bytes", "used_duration_display", "used_seconds", "used_traffic_display"]
                && all (`hasNull` plan) ["used_bytes", "used_seconds", "details_text", "cost_display", "balance_display"]
                && field "cost_yuan" plan == Number 0.1234
                && field "balance_yuan" plan == Number 10.9876
                && field "used_traffic_display" plan == String "1.23G"
                && field "used_duration_display" plan == String "1小时2分"
        _ -> False
    assert "zero online devices is a normal list, not a missing account" $
        null (overviewDevices empty)
            && field "count" (devicesJSON empty) == Number 0
            && field "devices" (devicesJSON empty) == toJSON ([] :: [Value])
            && hasNull "display_name" (field "account" $ infoJSON empty)
    assert "monetary zero distinct from missing and counters not inferred even from zero" $ case array $ field "products" $ infoJSON empty of
        [plan] -> field "cost_yuan" plan == Number 0 && field "balance_yuan" plan == Number 0 && all (`hasNull` plan) ["used_bytes", "used_seconds", "billing_policy"]
        _ -> False
    assert "IPv6 and duplicate sessions retained in source order" $
        map deviceIP (overviewDevices multiple) == ["2001:db8::1", "2001:db8::1", "192.0.2.9"]
            && field "count" (devicesJSON multiple) == Number 3
    assert "missing optional device values null and minutes normalized" $ case array $ field "devices" $ devicesJSON multiple of
        [first, second, third] ->
            hasNull "mac" first
                && field "connected_at" first == String "2026-10-02 10:20:00"
                && all (`hasNull` second) ["mac", "connected_at", "billing_policy"]
                && all (`hasNull` third) ["mac", "connected_at", "billing_policy"]
        _ -> False
    assert "each product keeps own amount and visible details only" $ case array $ field "products" $ infoJSON multiple of
        [first, second, third] ->
            field "balance_yuan" first == Number 100.1234
                && field "cost_yuan" first == Number (-0.0001)
                && field "details_text" first == String "可见 套餐 & 详情"
                && hasNull "cost_yuan" second
                && hasNull "balance_yuan" second
                && field "cost_display" second == String "约 1 元"
                && field "balance_display" second == String "¥ 20.00"
                && all (`hasNull` second) ["used_bytes", "used_traffic_display", "used_seconds", "used_duration_display", "billing_policy", "details_text"]
                && all (`hasNull` third) ["cost_yuan", "cost_display", "balance_yuan", "balance_display", "used_bytes", "used_traffic_display", "used_seconds", "used_duration_display", "billing_policy", "details_text"]
        _ -> False
    let normalText = renderInfo normal
        multipleText = renderInfo multiple <> renderDevices multiple
    assert "text retains all money decimals and formatted counter displays" $
        all (`T.isInfixOf` normalText) ["0.1234", "10.9876", "1.23G", "1小时2分", "未提供"]
    assert "text preserves separate balances and money fallback" $
        all (`T.isInfixOf` multipleText) ["100.1234", "-0.0001", "¥ 20.00", "约 1 元", "未提供"]
    assert "operation links and hidden content not included in outputs" $
        all (\sentinel -> not $ sentinel `T.isInfixOf` multipleText) ["OPERATION-SENTINEL", "/home/delete", "HIDDEN-SENTINEL", "SCRIPT-SENTINEL", "STYLE-SENTINEL", "ICON-SENTINEL", "TEMPLATE-SENTINEL"]
    let (beforeDevice, deviceAndAfter) = T.breakOn "<table id=\"devices\"" normalHTML
        (deviceBody, ending) = T.breakOn "</table>" deviceAndAfter
        withoutDevice = beforeDevice <> T.drop (T.length "</table>") ending
    reordered <- right $ parse $ withoutDevice <> deviceBody <> "</table>"
    assert "table order irrelevant" $ reordered == normal
    noName <- right $ parse $ T.replace "<li class=\"list-group-item\"><label class=\"list-group-label\">姓名</label>测试 &amp; 用户</li>" "" normalHTML
    assert "absent account name is optional" $ accountDisplayName (overviewAccount noName) == Nothing
    noProducts <- right $ parse $ replaceBody "products" "<tr><td colspan=\"8\"><div class=\"empty\">没有找到数据。</div></td></tr>" normalHTML
    assert "empty product list allowed with intact table" $ null $ overviewProducts noProducts
    let failures =
            [ ("login page", "<html><form id='login'>登录</form></html>")
            , ("service exception", "<title>服务异常</title>")
            , ("missing identity wrapper", T.replace "query-user" "other-user" normalHTML)
            , ("missing username label", T.replace ">用户名</label>" ">其他字段</label>" normalHTML)
            , ("empty username", T.replace "</label>fixture-user" "</label>" normalHTML)
            , ("missing status label", T.replace ">状态</label>" ">其他字段</label>" normalHTML)
            , ("unknown status", T.replace ">正常</a>" ">未知</a>" normalHTML)
            , ("duplicate identity label", T.replace "</ul>" "<li class=\"list-group-item\"><label class=\"list-group-label\">用户名</label>another-user</li></ul>" normalHTML)
            , ("missing device table", withoutDevice)
            , ("missing product table", T.replace "id=\"products\" class=\"kv-grid-table table\"" "id=\"products\" class=\"other\"" normalHTML)
            , ("changed device header", T.replace ">IP地址</th>" ">地址</th>" normalHTML)
            , ("changed product column", T.replace "data-col-seq=\"11\"" "data-col-seq=\"99\"" normalHTML)
            , ("missing body", T.replace "tbody" "unknown-body" normalHTML)
            , ("unkeyed data row", T.replace "data-key=\"fictional-session\"" "" normalHTML)
            , ("empty IP", T.replace ">192.0.2.1<" "><" normalHTML)
            , ("empty product ID", T.replace ">plan-01<" "><" normalHTML)
            , ("empty product name", T.replace ">基础 &amp; 套餐<" "><" normalHTML)
            , ("duplicated cell", T.replace "<td data-col-seq=\"1\">192.0.2.1</td>" "<td data-col-seq=\"1\">192.0.2.1</td><td data-col-seq=\"1\">192.0.2.2</td>" normalHTML)
            , ("ambiguous core tables", normalHTML <> deviceBody <> "</table>")
            ]
    mapM_ (\(label, body) -> assert ("reject malformed overview: " <> label) $ isLeft $ parse body) failures
    assert "invalid UTF-8 rejected" $ isLeft $ parseOverview $ BS.pack [255]
    fakeRaw <- right $ parse $ T.replace "<td data-col-seq=\"3\">1.23G</td>" "<td data-col-seq=\"3\" data-bytes=\"1320702443\">1.23G<span hidden>1320702443</span></td>" normalHTML
    assert "unverified attributes and hidden counters never supply exact bytes" $
        map productTraffic (overviewProducts fakeRaw) == [Just $ ObservedCounter "1.23G" Nothing]
    hostile <- right $ parse $ T.replace "fixture-user" "fixture\ESC[31m|user\x009b\x007f\n\t" normalHTML
    let hostileText = renderInfo hostile <> renderDevices hostile
    assert "terminal controls stripped except renderer line endings" $
        not $
            T.any (\c -> isControl c && c /= '\n') hostileText
    assert "page text cannot inject table separators" $
        not $
            "|user" `T.isInfixOf` hostileText
    assert "JSON retains structured source text rather than terminal mutation" $
        "\ESC" `T.isInfixOf` accountUsername (overviewAccount hostile)
    hostileFields <- right $ parse $ foldr (\value -> T.replace value (value <> "\ESC[31m|injected\x009b\x007f\n\t")) multipleHTML ["2001:db8::1", "套餐 A", "策略 &amp; A", "可见", "约 1 元", "¥ 20.00"]
    let allFieldsText = renderInfo hostileFields <> renderDevices hostileFields
    assert "device, product, details and formatted money text sanitized" $
        not (T.any (\c -> isControl c && c /= '\n') allFieldsText) && not ("|injected" `T.isInfixOf` allFieldsText)
    assert "page newlines cannot introduce output rows" $
        length (T.lines allFieldsText) == length (T.lines $ renderInfo multiple <> renderDevices multiple)
    putStrLn "Overview parsing, optional values, exact money, JSON and safe rendering checks passed."
  where
    keys (Object fields) = sort $ map Key.toText $ KM.keys fields
    keys _ = []

replaceBody :: Text -> Text -> Text -> Text
replaceBody identifier replacement html =
    let (before, table) = T.breakOn ("<table id=\"" <> identifier <> "\"") html
        (heading, body) = T.breakOn "<tbody>" table
        (_, ending) = T.breakOn "</tbody>" body
     in before <> heading <> "<tbody>" <> replacement <> ending
