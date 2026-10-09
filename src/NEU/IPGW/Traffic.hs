{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Traffic (
    TrafficFilter (..),
    noTrafficFilter,
    validateTrafficFilter,
    matchesTrafficFilter,
    trafficFilterJSON,
    renderTrafficFilter,
    TrafficGroup,
    trafficGroups,
    trafficGroupJSON,
    renderTrafficGroups,
) where

import Control.Monad (unless)
import Data.Aeson (Value, object, (.=))
import Data.List (foldl')
import Data.Map.Strict qualified as M
import Data.Scientific (Scientific)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime)
import NEU.IPGW.HTML (money, safe, timeText)
import NEU.IPGW.Logs (LogEntry (..), humanBytes)

data TrafficFilter = TrafficFilter
    { filterIPs :: [Text]
    , filterProducts :: [Text]
    , filterMinBytes :: Maybe Integer
    , filterMaxBytes :: Maybe Integer
    , filterMinSeconds :: Maybe Integer
    , filterMaxSeconds :: Maybe Integer
    }
    deriving (Eq, Show)

noTrafficFilter :: TrafficFilter
noTrafficFilter = TrafficFilter [] [] Nothing Nothing Nothing Nothing

validateTrafficFilter :: TrafficFilter -> Either Text ()
validateTrafficFilter f = do
    unless (all (not . T.null . T.strip) $ filterIPs f) $ Left "IP 筛选值不能为空"
    unless (all (not . T.null . T.strip) $ filterProducts f) $ Left "产品筛选值不能为空"
    range "流量" (filterMinBytes f) (filterMaxBytes f)
    range "时长" (filterMinSeconds f) (filterMaxSeconds f)
  where
    range label lo hi = do
        unless (maybe True (>= 0) lo && maybe True (>= 0) hi) $ Left $ label <> "筛选值不能为负数"
        unless (case (lo, hi) of (Just a, Just b) -> a <= b; _ -> True) $ Left $ label <> "筛选下限不能大于上限"

matchesTrafficFilter :: TrafficFilter -> LogEntry -> Bool
matchesTrafficFilter f TrafficEntry {address = ip, productName = product, bytes = b, seconds = s} =
    selected (filterIPs f) ip
        && selected (filterProducts f) product
        && within (filterMinBytes f) (filterMaxBytes f) b
        && within (filterMinSeconds f) (filterMaxSeconds f) s
  where
    selected values value = null values || value `elem` values
    within lo hi value = maybe True (<= value) lo && maybe True (>= value) hi
matchesTrafficFilter _ _ = False

trafficFilterJSON :: TrafficFilter -> Value
trafficFilterJSON f =
    object
        [ "ips" .= filterIPs f
        , "products" .= filterProducts f
        , "min_bytes" .= filterMinBytes f
        , "max_bytes" .= filterMaxBytes f
        , "min_seconds" .= filterMinSeconds f
        , "max_seconds" .= filterMaxSeconds f
        ]

renderTrafficFilter :: TrafficFilter -> [Text]
renderTrafficFilter f =
    ["IP：" <> T.intercalate "、" (map safe $ filterIPs f) | not $ null $ filterIPs f]
        ++ ["产品：" <> T.intercalate "、" (map safe $ filterProducts f) | not $ null $ filterProducts f]
        ++ [label <> number value | (label, Just value) <- bounds]
  where
    bounds =
        [ ("每条记录最小字节数：", filterMinBytes f)
        , ("每条记录最大字节数：", filterMaxBytes f)
        , ("每条记录最小时长（秒）：", filterMinSeconds f)
        , ("每条记录最大时长（秒）：", filterMaxSeconds f)
        ]

data TrafficGroup = TrafficGroup
    { groupIP :: !Text
    , groupProducts :: !(S.Set Text)
    , groupRecords :: !Integer
    , groupBytes :: !Integer
    , groupSeconds :: !Integer
    , groupCost :: !Scientific
    , groupFirstStarted :: !LocalTime
    , groupLastEnded :: !LocalTime
    }
    deriving (Eq, Show)

-- Raw IPs are session-log keys, not stable device identities. Callers filter
-- individual records before this fold; no current IP/MAC mapping is consulted.
trafficGroups :: [LogEntry] -> [TrafficGroup]
trafficGroups = M.elems . foldl' add M.empty
  where
    add groups TrafficEntry {address = ip, productName = product, bytes = b, seconds = s, cost = c, started = start, ended = end} =
        M.alter update ip groups
      where
        update Nothing = Just $ TrafficGroup ip (S.singleton product) 1 b s c start end
        update (Just g) =
            Just $
                g
                    { groupProducts = S.insert product $ groupProducts g
                    , groupRecords = groupRecords g + 1
                    , groupBytes = groupBytes g + b
                    , groupSeconds = groupSeconds g + s
                    , groupCost = groupCost g + c
                    , groupFirstStarted = min start $ groupFirstStarted g
                    , groupLastEnded = max end $ groupLastEnded g
                    }
    add groups _ = groups

trafficGroupJSON :: TrafficGroup -> Value
trafficGroupJSON g =
    object
        [ "ip" .= groupIP g
        , "products" .= S.toAscList (groupProducts g)
        , "records" .= groupRecords g
        , "bytes" .= groupBytes g
        , "seconds" .= groupSeconds g
        , "cost_yuan" .= groupCost g
        , "first_started_at" .= timeText (groupFirstStarted g)
        , "last_ended_at" .= timeText (groupLastEnded g)
        ]

renderTrafficGroups :: [TrafficGroup] -> Text
renderTrafficGroups groups =
    T.unlines $
        ["IP | 产品 | 记录数 | 流量（字节） | 时长（秒） | 消费（元） | 最早上线 | 最晚下线"]
            ++ map row groups
            ++ ["无匹配记录" | null groups]
            ++ [ "统计（仅上述记录）："
               , "IP 数：" <> number (length groups)
               , "记录数：" <> number records
               , "流量合计：" <> byteText totalBytes
               , "时长合计：" <> number totalSeconds <> " 秒"
               , "消费合计：" <> money totalCost <> " 元"
               ]
  where
    (records, totalBytes, totalSeconds, totalCost) = foldl' total (0, 0, 0, 0) groups
    total (!n, !b, !s, !c) g = (n + groupRecords g, b + groupBytes g, s + groupSeconds g, c + groupCost g)
    row g =
        T.intercalate " | " $
            map
                safe
                [ groupIP g
                , T.intercalate "、" $ S.toAscList $ groupProducts g
                , number $ groupRecords g
                , byteText $ groupBytes g
                , number $ groupSeconds g
                , money $ groupCost g
                , timeText $ groupFirstStarted g
                , timeText $ groupLastEnded g
                ]

number :: (Show a) => a -> Text
number = T.pack . show

byteText :: Integer -> Text
byteText n = humanBytes n <> " (" <> number n <> ")"
