{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Logs (
    LogKind (..),
    LogEntry (..),
    LogPage (..),
    parseLogPage,
    entryTime,
    entryID,
    entryBytes,
    kindName,
    logPath,
    entryJSON,
    statistics,
    renderLogs,
    humanBytes,
) where

import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Map.Strict qualified as M
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (LocalTime)
import NEU.IPGW.HTML
import Text.HTML.TagSoup (Tag, parseTags)
import Text.Read (readMaybe)

-- Billing is the settlement ledger, not the payment/recharge ledger.
data LogKind = TrafficLog | BillingLog | PaymentLog deriving (Eq, Show)
data LogEntry
    = TrafficEntry
        { recordID :: Text
        , username :: Text
        , started :: LocalTime
        , ended :: LocalTime
        , address :: Text
        , productName :: Text
        , balance :: Scientific
        , bytes :: Integer
        , seconds :: Integer
        , cost :: Scientific
        }
    | BillingEntry
        { recordID :: Text
        , username :: Text
        , settled :: LocalTime
        , cost :: Scientific
        , realtimeCost :: Scientific
        , bytes :: Integer
        , connections :: Integer
        }
    | PaymentEntry
        { recordID :: Text
        , username :: Text
        , paidAt :: LocalTime
        , amountYuan :: Scientific
        , paymentType :: Text
        }
    deriving (Eq, Show)
data LogPage = LogPage
    { totalRecords :: Int
    , firstRecord :: Int
    , lastRecord :: Int
    , pageEntries :: [LogEntry]
    }
    deriving (Eq, Show)

kindName :: LogKind -> Text
kindName TrafficLog = "traffic"
kindName BillingLog = "billing"
kindName PaymentLog = "payment"
logPath :: LogKind -> Text
logPath TrafficLog = "/log/detail"
logPath BillingLog = "/log/check-out"
logPath PaymentLog = "/log/pay"
entryTime :: LogEntry -> LocalTime
entryTime TrafficEntry {started = t} = t
entryTime BillingEntry {settled = t} = t
entryTime PaymentEntry {paidAt = t} = t
entryID :: LogEntry -> Text
entryID = recordID
entryBytes :: LogEntry -> Maybe Integer
entryBytes TrafficEntry {bytes = n} = Just n
entryBytes BillingEntry {bytes = n} = Just n
entryBytes PaymentEntry {} = Nothing

-- Only use the observed raw columns, and verify their labels before decoding.
-- Display columns are rounded and must never be used to reconstruct counters.
parseLogPage :: LogKind -> BS.ByteString -> Either Text LogPage
parseLogPage kind body = do
    html <- either (const $ Left "日志页面不是有效 UTF-8") Right $ TE.decodeUtf8' body
    let tags = parseTags html
        tables = [inside | (attrs, inside) <- elements "table" tags, hasClass "kv-grid-table" attrs]
    table <- case tables of [t] -> Right t; _ -> Left "无法识别日志表格；会话可能已过期或页面结构已变更"
    let heads = [(col attrs, clean inside) | (attrs, inside) <- elements "th" table]
        expected = case kind of
            TrafficLog -> [("0", "用户名"), ("1", "上线时间"), ("2", "下线时间"), ("5", "IP地址"), ("10", "产品名称"), ("12", "当前余额"), ("16", "入流量"), ("18", "时长"), ("20", "消费额")]
            BillingLog -> [("0", "ID"), ("1", "用户名"), ("2", "费用"), ("3", "实时费用"), ("6", "总流量"), ("10", "使用次数"), ("12", "结算时间")]
            PaymentLog -> [("0", "ID"), ("1", "用户名"), ("2", "缴费金额"), ("3", "缴费类型"), ("6", "缴费时间")]
    unless (all (\(k, v) -> lookup k heads == Just v && length (filter ((== k) . fst) heads) == 1) expected) $
        Left "日志列定义已变化，无法可靠统计"
    let rawRows = [(key, cells) | (attrs, cells) <- elements "tr" table, Just key <- [lookup "data-key" attrs]]
    rows <- traverse (parseRow kind) rawRows
    let summaries = [clean inside | (attrs, inside) <- elements "div" tags, hasClass "summary" attrs]
        empty = any (\(attrs, inside) -> hasClass "empty" attrs && clean inside `elem` ["没有找到数据。", "没有找到数据", "No results found."]) (elements "div" table)
    (first, lastOne, total) <- case summaries of
        [summary] -> case numbers summary of
            Just [a, b, c] | a >= 1 && b >= a && c >= b -> Right (a, b, c)
            _ -> Left "无法识别日志分页总数"
        [] | null rows && empty -> Right (0, 0, 0)
        _ -> Left "日志缺少可信的分页信息"
    unless ((total == 0 && null rows) || length rows == lastOne - first + 1) $ Left "日志记录数与分页信息不一致"
    pure $ LogPage total first lastOne rows
  where
    numbers = traverse (readMaybe . T.unpack) . filter (not . T.null) . T.split (not . isDigit)

parseRow :: LogKind -> (Text, [Tag Text]) -> Either Text LogEntry
parseRow kind (key, tags) = do
    let pairs = [(col attrs, clean inside) | (attrs, inside) <- elements "td" tags]
        cells = M.fromList pairs
        field k = maybe (Left $ "日志缺少字段列 " <> k) Right $ M.lookup k cells
        count k = field k >>= natural
        amount k = field k >>= decimal
        time k = field k >>= timestamp
    unless (not (T.null key) && length pairs == M.size cells) $ Left "日志记录标识或列重复"
    case kind of
        TrafficLog -> do
            start <- time "1"
            end <- time "2"
            unless (end >= start) $ Left "日志下线时间早于上线时间"
            TrafficEntry key <$> field "0" <*> pure start <*> pure end <*> field "5" <*> field "10" <*> amount "12" <*> count "16" <*> count "18" <*> amount "20"
        BillingLog -> do
            identifier <- field "0"
            unless (identifier == key) $ Left "结算日志 ID 与行标识不一致"
            BillingEntry key <$> field "1" <*> time "12" <*> amount "2" <*> amount "3" <*> count "6" <*> count "10"
        PaymentLog -> do
            identifier <- field "0"
            unless (identifier == key) $ Left "充值日志 ID 与行标识不一致"
            PaymentEntry key <$> field "1" <*> time "6" <*> amount "2" <*> field "3"

entryJSON :: LogEntry -> Value
entryJSON e@TrafficEntry {} =
    object
        [ "id" .= recordID e
        , "username" .= username e
        , "started_at" .= timeText (started e)
        , "ended_at" .= timeText (ended e)
        , "ip" .= address e
        , "product" .= productName e
        , "balance_yuan" .= balance e
        , "bytes" .= bytes e
        , "seconds" .= seconds e
        , "cost_yuan" .= cost e
        ]
entryJSON e@BillingEntry {} =
    object
        [ "id" .= recordID e
        , "username" .= username e
        , "settled_at" .= timeText (settled e)
        , "cost_yuan" .= cost e
        , "realtime_cost_yuan" .= realtimeCost e
        , "bytes" .= bytes e
        , "connections" .= connections e
        ]
entryJSON e@PaymentEntry {} =
    object
        [ "id" .= recordID e
        , "username" .= username e
        , "paid_at" .= timeText (paidAt e)
        , "amount_yuan" .= amountYuan e
        , "payment_type" .= paymentType e
        ]

statistics :: LogKind -> [LogEntry] -> Value
statistics PaymentLog entries =
    object
        [ "records" .= length payments
        , "amount_yuan" .= sum (map amountYuan payments)
        , "by_type" .= [object ["payment_type" .= name, "records" .= count, "amount_yuan" .= amount] | (name, (count, amount)) <- paymentGroups payments]
        ]
  where
    payments = [e | e@PaymentEntry {} <- entries]
statistics kind entries =
    object $
        ["records" .= length entries, "bytes" .= sum [n | e <- entries, Just n <- [entryBytes e]], "cost_yuan" .= sum [n | e <- entries, n <- case e of TrafficEntry {cost = c} -> [c]; BillingEntry {cost = c} -> [c]; PaymentEntry {} -> []]]
            ++ case kind of
                TrafficLog -> ["seconds" .= sum [seconds e | e@TrafficEntry {} <- entries]]
                BillingLog -> ["realtime_cost_yuan" .= sum [realtimeCost e | e@BillingEntry {} <- entries], "connections" .= sum [connections e | e@BillingEntry {} <- entries]]

paymentGroups :: [LogEntry] -> [(Text, (Int, Scientific))]
paymentGroups entries = M.toAscList $ M.fromListWith combine [(paymentType e, (1, amountYuan e)) | e@PaymentEntry {} <- entries]
  where
    combine (n, a) (m, b) = (n + m, a + b)

number :: (Show a) => a -> Text
number = T.pack . show

humanBytes :: Integer -> Text
humanBytes bytesValue = scale 1 ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB"]
  where
    scale divisor (unit : next : rest)
        | bytesValue >= divisor * 1024 = scale (divisor * 1024) (next : rest)
        | otherwise = render divisor unit
    scale divisor [unit] = render divisor unit
    scale _ [] = number bytesValue <> " B"
    render 1 unit = number bytesValue <> " " <> unit
    render divisor unit =
        let hundredths = (bytesValue * 100 + divisor `div` 2) `div` divisor
            (whole, fraction) = hundredths `divMod` 100
         in number whole <> "." <> T.justifyRight 2 '0' (number fraction) <> " " <> unit

renderLogs :: LogKind -> [LogEntry] -> Text
renderLogs PaymentLog entries =
    T.unlines $
        ["缴费时间 | ID | 缴费类型 | 金额（元）"]
            ++ [T.intercalate " | " $ map safe [timeText $ paidAt e, recordID e, paymentType e, money $ amountYuan e] | e@PaymentEntry {} <- entries]
            ++ [if null payments then "无匹配记录" else "", "统计（仅上述记录）：", "记录数：" <> number (length payments), "金额合计：" <> money (sum $ map amountYuan payments) <> " 元"]
            ++ [safe name <> "：" <> number count <> " 条，" <> money amount <> " 元" | (name, (count, amount)) <- paymentGroups payments]
  where
    payments = [e | e@PaymentEntry {} <- entries]
renderLogs kind entries =
    T.unlines $
        [header]
            ++ map row entries
            ++ [if null entries then "无匹配记录" else "", "统计（仅上述记录）："]
            ++ ["记录数：" <> number (length entries), "总流量：" <> humanBytes totalBytes <> "（" <> number totalBytes <> " 字节）"]
            ++ case kind of
                TrafficLog -> ["总时长：" <> number (sum [seconds e | e@TrafficEntry {} <- entries]) <> " 秒", "消费额合计：" <> money totalCost <> " 元"]
                BillingLog -> ["费用合计：" <> money totalCost <> " 元", "实时费用合计：" <> money (sum [realtimeCost e | e@BillingEntry {} <- entries]) <> " 元", "使用次数：" <> number (sum [connections e | e@BillingEntry {} <- entries]), "费用与实时费用保留网站口径，分别统计，不相加。"]
  where
    totalBytes = sum [n | e <- entries, Just n <- [entryBytes e]]
    totalCost = sum [n | e <- entries, n <- case e of TrafficEntry {cost = c} -> [c]; BillingEntry {cost = c} -> [c]; PaymentEntry {} -> []]
    header = case kind of
        TrafficLog -> "上线时间 | 下线时间 | IP | 套餐 | 流量（字节） | 时长（秒） | 消费额（元） | 余额（元）"
        BillingLog -> "结算时间 | ID | 费用（元） | 实时费用（元） | 流量（字节） | 使用次数"
    row e@TrafficEntry {} =
        T.intercalate " | " $
            map
                safe
                [timeText $ started e, timeText $ ended e, address e, productName e, humanBytes (bytes e) <> " (" <> number (bytes e) <> ")", number $ seconds e, money $ cost e, money $ balance e]
    row e@BillingEntry {} =
        T.intercalate " | " $
            map
                safe
                [timeText $ settled e, recordID e, money $ cost e, money $ realtimeCost e, humanBytes (bytes e) <> " (" <> number (bytes e) <> ")", number $ connections e]
    row e@PaymentEntry {} = T.intercalate " | " $ map safe [timeText $ paidAt e, recordID e, paymentType e, money $ amountYuan e]
