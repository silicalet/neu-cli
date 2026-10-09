{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Usage (
    UsagePeriod (..),
    UsageQuery (..),
    UsagePoint (..),
    UsageData (..),
    UsageReport (..),
    validateUsageQuery,
    parseUsage,
    usageReport,
    usageJSON,
    renderUsage,
    usagePath,
) where

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecodeStrict', object, (.=))
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum, isLetter, isSpace)
import Data.Foldable (toList)
import Data.List (tails)
import Data.Map.Strict qualified as Map
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import NEU.IPGW.HTML (elements, safe)
import NEU.IPGW.Logs (humanBytes)
import Text.HTML.TagSoup (Tag (..), parseTags)

data UsagePeriod = Daily | Monthly deriving (Eq, Show)

data UsageQuery = UsageQuery
    { usagePeriod :: UsagePeriod
    , usageFrom :: Maybe Text
    , usageTo :: Maybe Text
    }
    deriving (Eq, Show)

data UsagePoint = UsagePoint
    { sourceIndex :: Int
    , pointPeriod :: Text
    , pointBytes :: Integer
    }
    deriving (Eq, Show)

data UsageData = UsageData
    { usageDataPeriod :: UsagePeriod
    , usagePoints :: [UsagePoint]
    }
    deriving (Eq, Show)

data UsageReport = UsageReport
    { usageReportQuery :: UsageQuery
    , usageSourceData :: UsageData
    , usageEntries :: [UsagePoint]
    }
    deriving (Eq, Show)

usagePath :: UsagePeriod -> Text
usagePath Daily = "/statistic/index"
usagePath Monthly = "/statistic/flow"

periodName :: UsagePeriod -> Text
periodName Daily = "daily"
periodName Monthly = "monthly"

validateUsageQuery :: UsageQuery -> Either Text ()
validateUsageQuery query = do
    mapM_ (validPeriod $ usagePeriod query) $ usageFrom query
    mapM_ (validPeriod $ usagePeriod query) $ usageTo query
    case (usageFrom query, usageTo query) of
        (Just first, Just lastPeriod) | first > lastPeriod -> Left "usage 起始期间不能晚于结束期间"
        _ -> Right ()

validPeriod :: UsagePeriod -> Text -> Either Text ()
validPeriod period value = do
    let patternText = case period of
            Daily -> "dddd-dd-dd"
            Monthly -> "dddd-dd"
        matches actual expected = if expected == 'd' then actual >= '0' && actual <= '9' else actual == expected
        dayText = if period == Monthly then value <> "-01" else value
        parsed = parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dayText) :: Maybe Day
    unless (T.length value == T.length patternText && and (zipWith matches (T.unpack value) (T.unpack patternText)) && parsed /= Nothing) $
        Left $
            "usage 期间必须是有效的 " <> (if period == Daily then "YYYY-MM-DD" else "YYYY-MM")

-- The lexer only recognizes enough JavaScript syntax to locate literal assignments.
-- Strings and comments are opaque, so their contents cannot impersonate data or chart references.
data Token = WordToken Text | MarkToken Char | StringToken | ObjectStart Text deriving (Eq, Show)

lexScript :: Text -> Either Text [Token]
lexScript input = case T.uncons input of
    Nothing -> Right []
    Just (c, rest)
        | isSpace c -> lexScript rest
        | "//" `T.isPrefixOf` input -> lexScript $ T.dropWhile (/= '\n') $ T.drop 2 input
        | "/*" `T.isPrefixOf` input ->
            let (_, ending) = T.breakOn "*/" $ T.drop 2 input
             in if T.null ending then Left "usage 脚本注释未结束" else lexScript $ T.drop 2 ending
        | c == '\'' || c == '"' || c == '`' -> do
            after <- skipString c rest
            (StringToken :) <$> lexScript after
        | isLetter c || c == '_' || c == '$' ->
            let (name, after) = T.span (\x -> isAlphaNum x || x == '_' || x == '$') input
             in (WordToken name :) <$> lexScript after
        | c == '{' -> (ObjectStart input :) <$> lexScript rest
        | otherwise -> (MarkToken c :) <$> lexScript rest

skipString :: Char -> Text -> Either Text Text
skipString quote input = case T.uncons input of
    Nothing -> Left "usage 脚本字符串未结束"
    Just ('\\', rest) -> case T.uncons rest of
        Nothing -> Left "usage 脚本转义未结束"
        Just (_, after) -> skipString quote after
    Just (c, rest) | c == quote -> Right rest
    Just (_, rest) -> skipString quote rest

-- Scan the complete object rather than splitting at a semicolon. JSON strings
-- may contain braces, semicolons and escaped quotes; Aeson validates the result.
jsonObject :: Text -> Either Text Text
jsonObject input = go (0 :: Int) False False 0 input
  where
    go depth quoted escaped count remaining = case T.uncons remaining of
        Nothing -> Left "usage JSON 对象未结束"
        Just (c, rest)
            | quoted && escaped -> go depth True False (count + 1) rest
            | quoted && c == '\\' -> go depth True True (count + 1) rest
            | quoted && c == '"' -> go depth False False (count + 1) rest
            | quoted -> go depth True False (count + 1) rest
            | c == '"' -> go depth True False (count + 1) rest
            | c == '{' -> go (depth + 1) False False (count + 1) rest
            | c == '}' && depth == 1 -> Right $ T.take (count + 1) input
            | c == '}' -> go (depth - 1) False False (count + 1) rest
            | otherwise -> go depth False False (count + 1) rest

assignments :: [Token] -> [Text]
assignments = scan Nothing
  where
    scan previous (token@(WordToken "data") : MarkToken '=' : ObjectStart raw : rest)
        | previous /= Just (MarkToken '.') = raw : scan (Just token) rest
    scan _ (token : rest) = scan (Just token) rest
    scan _ [] = []

containsTokens :: [Token] -> [Token] -> Bool
containsTokens needle = any ((== needle) . take (length needle)) . tails

chartAssociated :: [Token] -> Bool
chartAssociated tokens =
    all
        (`containsTokens` tokens)
        [ [WordToken "data", MarkToken '.', WordToken "category"]
        , [WordToken "data", MarkToken '.', WordToken "series"]
        , [WordToken "echarts", MarkToken '.', WordToken "init", MarkToken '(']
        ]
        && containsTokens [MarkToken '.', WordToken "setOption", MarkToken '('] tokens

parseUsage :: UsagePeriod -> ByteString -> Either Text UsageData
parseUsage period body = do
    html <- either (const $ Left "usage 页面不是有效 UTF-8") Right $ TE.decodeUtf8' body
    let scripts = [T.concat [text | TagText text <- tags] | (_, tags) <- elements "script" $ parseTags html]
    candidates <- fmap concat $ mapM scriptCandidates scripts
    let associated = [value | (True, value) <- candidates]
        bySchema = [Right value | (_, Right value@(Object fields)) <- candidates, KM.member "category" fields && KM.member "series" fields]
        selected = if null associated then bySchema else associated
    value <- case selected of
        [candidate] -> candidate
        [] -> Left "usage 页面缺少可识别的图表数据"
        _ -> Left "usage 页面包含多个无法区分的图表数据对象"
    points <- decodePoints period value
    pure $ UsageData period points
  where
    scriptCandidates script = do
        tokens <- lexScript script
        pure [(chartAssociated tokens, decode raw) | raw <- assignments tokens]
    decode raw = do
        json <- jsonObject raw
        unless (assignmentEnds $ T.drop (T.length json) raw) $
            Left "usage 图表数据必须是独立的 JSON 对象赋值"
        either (const $ Left "usage 图表数据不是有效 JSON") Right $ eitherDecodeStrict' $ TE.encodeUtf8 json

assignmentEnds :: Text -> Bool
assignmentEnds raw
    | T.null rest || ";" `T.isPrefixOf` rest = True
    | "//" `T.isPrefixOf` rest = assignmentEnds $ T.dropWhile (/= '\n') $ T.drop 2 rest
    | "/*" `T.isPrefixOf` rest =
        let (_, ending) = T.breakOn "*/" $ T.drop 2 rest
         in not (T.null ending) && assignmentEnds (T.drop 2 ending)
    | otherwise = False
  where
    rest = T.stripStart raw

decodePoints :: UsagePeriod -> Value -> Either Text [UsagePoint]
decodePoints period (Object fields) = case (KM.lookup "category" fields, KM.lookup "series" fields) of
    (Just (Array categories), Just (Array series)) -> do
        unless (length categories == length series) $ Left "usage category 与 series 长度不一致"
        mapM point $ zip3 [0 ..] (toList categories) (toList series)
    _ -> Left "usage category 与 series 必须是数组"
  where
    point (index, String key, Number number) = do
        validPeriod period key
        -- The daily endpoint reports decimal kB; the monthly endpoint reports
        -- bytes. Verified against their same-month totals and the log counters;
        -- the shared chart formatter is not a reliable source-unit declaration.
        let scaled = case period of Daily -> number * 1000; Monthly -> number
        bytes <- case floatingOrInteger scaled :: Either Double Integer of
            Right integer | integer >= 0 -> Right integer
            _ -> Left "usage 流量换算后必须是非负整数字节"
        pure $ UsagePoint index key bytes
    point _ = Left "usage 期间必须是字符串，流量必须是非负 JSON Number"
decodePoints _ _ = Left "usage 图表数据必须是 JSON 对象"

usageReport :: UsageQuery -> UsageData -> Either Text UsageReport
usageReport query source = do
    validateUsageQuery query
    unless (usagePeriod query == usageDataPeriod source) $ Left "usage 查询期间与源数据类型不一致"
    pure $ UsageReport query source $ filter within $ usagePoints source
  where
    within point = maybe True (<= pointPeriod point) (usageFrom query) && maybe True (>= pointPeriod point) (usageTo query)

data Summary = Summary
    { distinctCount :: Int
    , duplicates :: [Text]
    , totalBytes :: Maybe Integer
    , peakBytes :: Maybe Integer
    , peakPeriods :: [Text]
    }

summarize :: [UsagePoint] -> Summary
summarize points =
    let counts = Map.fromListWith (+) [(pointPeriod point, 1 :: Int) | point <- points]
        repeated = Map.keys $ Map.filter (> 1) counts
        total = if null repeated then Just $ sum $ map pointBytes points else Nothing
        peak = if null repeated && not (null points) then Just $ maximum $ map pointBytes points else Nothing
     in Summary (Map.size counts) repeated total peak [pointPeriod point | point <- points, Just (pointBytes point) == peak]

aggregationStatus :: Summary -> Text
aggregationStatus summary = if null $ duplicates summary then "complete" else "ambiguous"

usageJSON :: UsageReport -> Value
usageJSON report =
    let query = usageReportQuery report
        source = usagePoints $ usageSourceData report
        sourceSummary = summarize source
        points = usageEntries report
        summary = summarize points
     in object
            [ "period" .= periodName (usagePeriod query)
            , "timezone" .= ("Asia/Shanghai" :: Text)
            , "from" .= usageFrom query
            , "to" .= usageTo query
            , "source_points" .= length source
            , "source_distinct_periods" .= distinctCount sourceSummary
            , "source_duplicate_periods" .= duplicates sourceSummary
            , "entries" .= map pointJSON points
            , "statistics"
                .= object
                    [ "points" .= length points
                    , "distinct_periods" .= distinctCount summary
                    , "duplicate_periods" .= duplicates summary
                    , "aggregation_status" .= aggregationStatus summary
                    , "total_bytes" .= totalBytes summary
                    , "peak_bytes" .= peakBytes summary
                    , "peak_periods" .= peakPeriods summary
                    ]
            , "warnings" .= (["duplicate_periods" :: Text | not $ null $ duplicates summary])
            ]
  where
    pointJSON point = object ["source_index" .= sourceIndex point, "period" .= pointPeriod point, "bytes" .= pointBytes point]

renderUsage :: UsageReport -> Text
renderUsage report =
    let query = usageReportQuery report
        source = usagePoints $ usageSourceData report
        points = usageEntries report
        sourceSummary = summarize source
        summary = summarize points
        header =
            [ "期间: " <> periodName (usagePeriod query) <> " | 时区: Asia/Shanghai"
            , "范围（含首尾，本地筛选）: " <> maybe "未限定" safe (usageFrom query) <> " 至 " <> maybe "未限定" safe (usageTo query)
            , "源数据点: " <> shown (length source) <> " | 不同期间: " <> shown (distinctCount sourceSummary)
            , "筛选后数据点: " <> shown (length points) <> " | 不同期间: " <> shown (distinctCount summary)
            , "source_index | 日期/月 | 流量（字节）"
            ]
        rows = [shown (sourceIndex point) <> " | " <> safe (pointPeriod point) <> " | " <> bytesText (pointBytes point) | point <- points]
        totals = case totalBytes summary of
            Nothing -> ["存在重复期间，暂不汇总: " <> T.intercalate ", " (map safe $ duplicates summary)]
            Just total ->
                [ "合计（仅实际返回且符合范围的数据点）: " <> bytesText total
                , "峰值: " <> maybe "未提供" bytesText (peakBytes summary) <> if null (peakPeriods summary) then "" else " | " <> T.intercalate ", " (map safe $ peakPeriods summary)
                ]
        sourceWarning = ["源数据重复期间: " <> T.intercalate ", " (map safe $ duplicates sourceSummary) | not $ null $ duplicates sourceSummary]
     in T.unlines $ header ++ rows ++ totals ++ sourceWarning
  where
    shown = T.pack . show

bytesText :: Integer -> Text
bytesText bytes = humanBytes bytes <> " (" <> T.pack (show bytes) <> ")"
