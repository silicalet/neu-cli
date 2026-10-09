{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Overview (
    Overview (..),
    AccountOverview (..),
    OnlineDevice (..),
    ProductOverview (..),
    ObservedCounter (..),
    parseOverview,
    infoJSON,
    devicesJSON,
    renderInfo,
    renderDevices,
) where

import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Maybe (mapMaybe)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (LocalTime)
import NEU.IPGW.HTML (clean, col, decimal, elements, hasClass, money, safe, timeText, timestamp)
import NEU.IPGW.Logs (humanBytes)
import Text.HTML.TagSoup (Tag (..), parseTags)

data Overview = Overview
    { overviewAccount :: AccountOverview
    , overviewDevices :: [OnlineDevice]
    , overviewProducts :: [ProductOverview]
    }
    deriving (Eq, Show)

data AccountOverview = AccountOverview
    { accountUsername :: Text
    , accountDisplayName :: Maybe Text
    , accountStatus :: Text
    }
    deriving (Eq, Show)

data OnlineDevice = OnlineDevice
    { deviceIP :: Text
    , deviceMAC :: Maybe Text
    , deviceConnectedAt :: Maybe LocalTime
    , deviceBillingPolicy :: Maybe Text
    }
    deriving (Eq, Show)

data ObservedCounter = ObservedCounter
    { counterDisplay :: Text
    , counterExact :: Maybe Integer
    }
    deriving (Eq, Show)

data ProductOverview = ProductOverview
    { productID :: Text
    , productName :: Text
    , productBillingPolicy :: Maybe Text
    , productTraffic :: Maybe ObservedCounter
    , productDuration :: Maybe ObservedCounter
    , productCost :: Maybe Scientific
    , productCostDisplay :: Maybe Text
    , productBalance :: Maybe Scientific
    , productBalanceDisplay :: Maybe Text
    , productDetails :: Maybe Text
    }
    deriving (Eq, Show)

-- Column identities are specific to /home, not the similarly styled log tables.
parseOverview :: BS.ByteString -> Either Text Overview
parseOverview body = do
    html <- either (const $ Left "首页不是有效 UTF-8") Right $ TE.decodeUtf8' body
    let tags = parseTags html
        accounts = [inside | (attrs, inside) <- elements "div" tags, hasClass "query-user" attrs]
        tables = [inside | (attrs, inside) <- elements "table" tags, hasClass "kv-grid-table" attrs]
    accountTags <- unique "无法识别首页账号；会话可能已过期或页面结构已变更" accounts
    account <- parseAccount accountTags
    devices <- matchingTable "在线设备" deviceHeaders tables >>= tableRows >>= traverse parseDevice
    products <- matchingTable "产品" productHeaders tables >>= tableRows >>= traverse parseProduct
    pure $ Overview account devices products
  where
    deviceHeaders = [("0", "用户名"), ("1", "IP地址"), ("3", "上线时间"), ("7", "计费策略"), ("9", "MAC地址")]
    productHeaders = [("0", "产品ID"), ("1", "产品名称"), ("2", "计费策略"), ("3", "已用流量"), ("4", "已用时长"), ("6", "消费额"), ("7", "产品余额"), ("11", "套餐详情")]

unique :: Text -> [a] -> Either Text a
unique _ [value] = Right value
unique message _ = Left message

matchingTable :: Text -> [(Text, Text)] -> [[Tag Text]] -> Either Text [Tag Text]
matchingTable label expected = unique ("无法可靠识别首页" <> label <> "表格") . filter matches
  where
    matches table =
        let heads = [(col attrs, visibleText inside) | (attrs, inside) <- elements "th" table]
         in all (\(key, value) -> filter ((== key) . fst) heads == [(key, value)]) expected

parseAccount :: [Tag Text] -> Either Text AccountOverview
parseAccount tags = do
    let fields =
            [ (visibleText label, clean $ omitElements isLabel inside)
            | (attrs, inside) <- elements "li" tags
            , hasClass "list-group-item" attrs
            , (labelAttrs, label) <- elements "label" inside
            , hasClass "list-group-label" labelAttrs
            ]
        values key = [value | (name, value) <- fields, name == key]
        required key = unique ("首页账号缺少或重复字段：" <> key) (values key) >>= requireText key
        optional key = case values key of [value] -> known value; _ -> Nothing
    AccountOverview <$> required "用户名" <*> pure (optional "姓名") <*> required "状态"
  where
    isLabel name attrs = name == "label" && hasClass "list-group-label" attrs

-- A missing body is not an empty result. Unidentified data rows likewise must
-- not silently turn a changed page into a zero-device result.
tableRows :: [Tag Text] -> Either Text [[Tag Text]]
tableRows table = do
    body <- unique "首页表格缺少或重复数据区" $ map snd $ elements "tbody" table
    rows <- traverse row $ elements "tr" body
    pure $ mapMaybe id rows
  where
    row (attrs, inside) = case lookup "data-key" attrs of
        Just key -> do
            _ <- requireText "行标识" key
            pure $ Just inside
        Nothing -> do
            let emptyMarker = any (\(cellAttrs, _) -> hasClass "empty" cellAttrs) $ elements "div" inside
                emptyRow = null (elements "td" inside) && T.null (visibleText inside)
            unless ((emptyMarker || emptyRow) && all (T.null . col . fst) (elements "td" inside)) $ Left "首页数据行缺少标识或空表标记"
            pure Nothing

type Cells = [(Text, Text)]

rowCells :: [Tag Text] -> Either Text Cells
rowCells tags = do
    let pairs = [(col attrs, visibleText inside) | (attrs, inside) <- elements "td" tags, not $ T.null $ col attrs]
    unless (all (\(key, _) -> length (filter ((== key) . fst) pairs) == 1) pairs) $ Left "首页数据列重复"
    pure pairs

requiredCell :: Text -> Cells -> Either Text Text
requiredCell key cells = maybe (Left $ "首页数据缺少字段列 " <> key) (requireText key) $ lookup key cells

requireText :: Text -> Text -> Either Text Text
requireText label value = maybe (Left $ "首页字段为空或未知：" <> label) Right $ known value

known :: Text -> Maybe Text
known value
    | T.toCaseFold trimmed `elem` ["", "-", "--", "—", "未知", "未提供", "n/a", "null", "unknown"] = Nothing
    | otherwise = Just trimmed
  where
    trimmed = T.strip value

optionalCell :: Text -> Cells -> Maybe Text
optionalCell key cells = lookup key cells >>= known

parseDevice :: [Tag Text] -> Either Text OnlineDevice
parseDevice tags = do
    cells <- rowCells tags
    ip <- requiredCell "1" cells
    let connected = optionalCell "3" cells >>= either (const Nothing) Just . timestamp
    pure $ OnlineDevice ip (optionalCell "9" cells) connected (optionalCell "7" cells)

parseProduct :: [Tag Text] -> Either Text ProductOverview
parseProduct tags = do
    cells <- rowCells tags
    identifier <- requiredCell "0" cells
    name <- requiredCell "1" cells
    let counter key = (\display -> ObservedCounter display Nothing) <$> optionalCell key cells
        amount key = case optionalCell key cells of
            Nothing -> (Nothing, Nothing)
            Just display -> case decimal display of
                Right exact -> (Just exact, Nothing)
                Left _ -> (Nothing, Just display)
        (cost, costDisplay) = amount "6"
        (balance, balanceDisplay) = amount "7"
    pure $ ProductOverview identifier name (optionalCell "2" cells) (counter "3") (counter "4") cost costDisplay balance balanceDisplay (optionalCell "11" cells)

visibleText :: [Tag Text] -> Text
visibleText = clean . omitElements (\_ _ -> False)

-- Read only DOM text that is already visible. In particular, script bodies,
-- hidden modal details and operation attributes are not account/product data.
omitElements :: (Text -> [(Text, Text)] -> Bool) -> [Tag Text] -> [Tag Text]
omitElements extra = go []
  where
    go _ [] = []
    go stack (tag : rest) = case tag of
        TagOpen name attrs ->
            let blocked = any snd stack || hidden name attrs || extra name attrs
                next = if name `elem` voidTags then stack else (name, blocked) : stack
             in (if blocked then id else (tag :)) $ go next rest
        TagClose name ->
            let next = case dropWhile ((/= name) . fst) stack of [] -> stack; _ : outer -> outer
             in (if any snd stack then id else (tag :)) $ go next rest
        _ -> (if any snd stack then id else (tag :)) $ go stack rest
    hidden name attrs =
        name `elem` ["script", "style", "template", "noscript"]
            || hasClass "hidden" attrs
            || hasClass "sr-only" attrs
            || any ((== "hidden") . fst) attrs
            || lookup "aria-hidden" attrs == Just "true"
            || any (`T.isInfixOf` style attrs) ["display:none", "visibility:hidden"]
    style = T.filter (not . (`elem` [' ', '\t', '\r', '\n'])) . T.toCaseFold . maybe "" id . lookup "style"
    voidTags = ["area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source", "track", "wbr"]

infoJSON :: Overview -> Value
infoJSON overview =
    object
        [ "account" .= accountJSON (overviewAccount overview)
        , "products" .= map productJSON (overviewProducts overview)
        , "online_device_count" .= length (overviewDevices overview)
        , "timezone" .= ("Asia/Shanghai" :: Text)
        ]

accountJSON :: AccountOverview -> Value
accountJSON account = object ["username" .= accountUsername account, "display_name" .= accountDisplayName account, "status" .= accountStatus account]

devicesJSON :: Overview -> Value
devicesJSON overview = object ["devices" .= map deviceJSON devices, "count" .= length devices, "timezone" .= ("Asia/Shanghai" :: Text)]
  where
    devices = overviewDevices overview

deviceJSON :: OnlineDevice -> Value
deviceJSON device =
    object
        [ "ip" .= deviceIP device
        , "mac" .= deviceMAC device
        , "connected_at" .= fmap timeText (deviceConnectedAt device)
        , "billing_policy" .= deviceBillingPolicy device
        ]

productJSON :: ProductOverview -> Value
productJSON plan =
    object
        [ "id" .= productID plan
        , "name" .= productName plan
        , "billing_policy" .= productBillingPolicy plan
        , "used_bytes" .= (productTraffic plan >>= counterExact)
        , "used_traffic_display" .= fmap counterDisplay (productTraffic plan)
        , "used_seconds" .= (productDuration plan >>= counterExact)
        , "used_duration_display" .= fmap counterDisplay (productDuration plan)
        , "cost_yuan" .= productCost plan
        , "balance_yuan" .= productBalance plan
        , "cost_display" .= productCostDisplay plan
        , "balance_display" .= productBalanceDisplay plan
        , "details_text" .= productDetails plan
        ]

renderInfo :: Overview -> Text
renderInfo overview =
    T.unlines $
        [ line ["用户名", accountUsername account]
        , line ["姓名", provided $ accountDisplayName account]
        , line ["状态", accountStatus account]
        , line ["在线设备", number $ length $ overviewDevices overview]
        , "时区: Asia/Shanghai"
        , line ["产品 ID", "产品名称", "计费策略", "已用流量", "已用时长", "消费额（元）", "产品余额（元）", "套餐详情"]
        ]
            ++ map renderProduct (overviewProducts overview)
  where
    account = overviewAccount overview
    renderProduct plan =
        line
            [ productID plan
            , productName plan
            , provided $ productBillingPolicy plan
            , counterText (\bytes -> humanBytes bytes <> " (" <> number bytes <> " 字节)") $ productTraffic plan
            , counterText (\seconds -> number seconds <> " 秒") $ productDuration plan
            , amountText (productCost plan) (productCostDisplay plan)
            , amountText (productBalance plan) (productBalanceDisplay plan)
            , provided $ productDetails plan
            ]

renderDevices :: Overview -> Text
renderDevices overview =
    T.unlines $
        ["在线设备: " <> number (length devices), "时区: Asia/Shanghai", line ["IP地址", "MAC地址", "上线时间", "计费策略"]]
            ++ [line [deviceIP device, provided $ deviceMAC device, provided $ timeText <$> deviceConnectedAt device, provided $ deviceBillingPolicy device] | device <- devices]
  where
    devices = overviewDevices overview

provided :: Maybe Text -> Text
provided = maybe "未提供" id

counterText :: (Integer -> Text) -> Maybe ObservedCounter -> Text
counterText render = maybe "未提供" (\counter -> maybe (counterDisplay counter) render $ counterExact counter)

amountText :: Maybe Scientific -> Maybe Text -> Text
amountText exact display = maybe (provided display) money exact

line :: [Text] -> Text
line = T.intercalate " | " . map safe

number :: (Show a) => a -> Text
number = T.pack . show
