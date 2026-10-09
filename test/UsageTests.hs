{-# LANGUAGE OverloadedStrings #-}

module UsageTests (usageTests) where

import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecode, encode, toJSON)
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import NEU.IPGW.Usage

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

field :: Key -> Value -> Value
field key (Object fields) = maybe Null id $ KM.lookup key fields
field _ _ = Null

array :: Value -> [Value]
array (Array values) = toList values
array _ = []

statistics :: UsageReport -> Value
statistics = field "statistics" . usageJSON

script :: Text -> Text
script body = "<script>" <> body <> "</script>"

chart :: Text
chart = "var myChart=echarts.init(document.getElementById('main'));var option={xAxis:{data:data.category},series:[{data:data.series}]};myChart.setOption(option);"

page :: Text -> Text
page json = script $ "var data = " <> json <> ";" <> chart

parse :: UsagePeriod -> Text -> Either Text UsageData
parse period = parseUsage period . TE.encodeUtf8

usageTests :: IO ()
usageTests = do
    dailyHTML <- T.readFile "test/fixtures/usage-daily.html"
    monthlyHTML <- T.readFile "test/fixtures/usage-monthly.html"
    daily <- right $ parse Daily dailyHTML
    monthly <- right $ parse Monthly monthlyHTML
    let allDaily = UsageQuery Daily Nothing Nothing
        allMonthly = UsageQuery Monthly Nothing Nothing
    assert "daily decimal kB convert to exact bytes without changing source order" $
        usagePoints daily == [UsagePoint 0 "2026-10-01" 123000, UsagePoint 1 "2026-10-03" 9007199254742017000, UsagePoint 2 "2026-10-08" 9007199254742017000]
    dailyReport <- right $ usageReport allDaily daily
    let dailyJSON = usageJSON dailyReport
        dailyStats = statistics dailyReport
    assert "JSON top-level contract contains exactly required fields" $
        case dailyJSON of
            Object fields -> all (`KM.member` fields) ["period", "timezone", "from", "to", "source_points", "source_distinct_periods", "source_duplicate_periods", "entries", "statistics", "warnings"] && KM.size fields == 10
            _ -> False
    assert "unbounded range and timezone explicit" $ field "from" dailyJSON == Null && field "to" dailyJSON == Null && field "timezone" dailyJSON == String "Asia/Shanghai" && field "period" dailyJSON == String "daily"
    assert "large exact sum and all tied peaks preserved" $
        field "total_bytes" dailyStats == Number 18014398509484157000
            && field "peak_bytes" dailyStats == Number 9007199254742017000
            && field "peak_periods" dailyStats == toJSON (["2026-10-03", "2026-10-08"] :: [Text])
            && field "aggregation_status" dailyStats == String "complete"
    assert "human units and normalized exact bytes displayed" $ "120.12 KiB (123000)" `T.isInfixOf` renderUsage dailyReport && "9007199254742017000" `T.isInfixOf` renderUsage dailyReport
    bounded <- right $ usageReport (UsageQuery Daily (Just "2026-10-03") (Just "2026-10-08")) daily
    assert "daily boundaries inclusive and source indices immutable" $ map sourceIndex (usageEntries bounded) == [1, 2] && field "source_points" (usageJSON bounded) == Number 3 && field "points" (statistics bounded) == Number 2
    single <- right $ usageReport (UsageQuery Daily (Just "2026-10-03") (Just "2026-10-03")) daily
    assert "single-day exact boundary and entry JSON" $
        map sourceIndex (usageEntries single) == [1] && case array $ field "entries" $ usageJSON single of
            [entry] -> field "source_index" entry == Number 1 && field "period" entry == String "2026-10-03" && field "bytes" entry == Number 9007199254742017000
            _ -> False
    gap <- right $ usageReport (UsageQuery Daily (Just "2026-10-04") (Just "2026-10-07")) daily
    assertEmpty "missing days are not filled" gap
    lowerOnly <- right $ usageReport (UsageQuery Daily (Just "2026-10-08") Nothing) daily
    upperOnly <- right $ usageReport (UsageQuery Daily Nothing (Just "2026-10-01")) daily
    assert "one-sided ranges are inclusive" $ map sourceIndex (usageEntries lowerOnly) == [2] && map sourceIndex (usageEntries upperOnly) == [0]
    empty <- right $ parse Daily $ page "{\"category\":[],\"series\":[]}"
    emptyReport <- right $ usageReport allDaily empty
    assertEmpty "valid empty graph accepted" emptyReport
    monthReport <- right $ usageReport allMonthly monthly
    let monthStats = statistics monthReport
    assert "different and identical duplicate values never deduplicated" $
        length (usageEntries monthReport) == 6
            && map pointBytes (usageEntries monthReport) == [1024, 2048, 4096, 8192, 8192, 4096]
            && field "distinct_periods" monthStats == Number 4
            && field "duplicate_periods" monthStats == toJSON (["2026-01", "2026-03"] :: [Text])
    assertAmbiguous "monthly duplicates prohibit aggregate and peaks" monthReport
    assert "ambiguous text keeps all source rows and explanatory warning" $ "存在重复期间，暂不汇总" `T.isInfixOf` renderUsage monthReport && "4 | 2026-03" `T.isInfixOf` renderUsage monthReport
    oneMonth <- right $ usageReport (UsageQuery Monthly (Just "2026-02") (Just "2026-02")) monthly
    assert "filter removing duplicates allows totals and retains source metadata" $
        map sourceIndex (usageEntries oneMonth) == [2]
            && field "total_bytes" (statistics oneMonth) == Number 4096
            && field "aggregation_status" (statistics oneMonth) == String "complete"
            && field "source_points" (usageJSON oneMonth) == Number 6
            && field "source_distinct_periods" (usageJSON oneMonth) == Number 4
            && field "source_duplicate_periods" (usageJSON oneMonth) == toJSON (["2026-01", "2026-03"] :: [Text])
            && field "warnings" (usageJSON oneMonth) == toJSON ([] :: [Text])
    monthBoundaries <- right $ usageReport (UsageQuery Monthly (Just "2026-02") (Just "2026-04")) monthly
    assert "monthly boundaries inclusive without renumbering" $ map sourceIndex (usageEntries monthBoundaries) == [2, 3, 4, 5]
    assertAmbiguous "remaining identical duplicate values still ambiguous" monthBoundaries
    duplicateDaily <- right $ parse Daily $ page "{\"category\":[\"2026-10-01\",\"2026-10-01\"],\"series\":[0,0]}"
    duplicateDailyReport <- right $ usageReport allDaily duplicateDaily
    assertAmbiguous "daily duplicate dates use conservative aggregation" duplicateDailyReport
    zero <- right $ parse Daily $ page "{\"category\":[\"2026-10-01\"],\"series\":[0]}"
    zeroReport <- right $ usageReport allDaily zero
    assert "real zero differs from no peak" $ field "peak_bytes" (statistics zeroReport) == Number 0 && field "peak_periods" (statistics zeroReport) == toJSON (["2026-10-01"] :: [Text])
    mapM_
        (\query -> assert "invalid range fails validation" $ isLeft $ validateUsageQuery query)
        [ UsageQuery Daily (Just "2026-2-01") Nothing
        , UsageQuery Daily (Just "2026-02-29") Nothing
        , UsageQuery Daily (Just "2026-04-31") Nothing
        , UsageQuery Daily (Just "2026-10") Nothing
        , UsageQuery Daily (Just "2026-10-01 ") Nothing
        , UsageQuery Daily (Just "2026-10-01\n") Nothing
        , UsageQuery Daily Nothing (Just " 2026-10-01")
        , UsageQuery Daily (Just "2026-10-09") (Just "2026-10-08")
        , UsageQuery Monthly (Just "2026-00") Nothing
        , UsageQuery Monthly (Just "2026-13") Nothing
        , UsageQuery Monthly (Just "2026-1") Nothing
        , UsageQuery Monthly (Just "2026-10-01") Nothing
        , UsageQuery Monthly (Just "2026-11") (Just "2026-10")
        ]
    _ <- right $ validateUsageQuery $ UsageQuery Daily (Just "2024-02-29") (Just "2024-02-29")
    assert "report rejects invalid range even without CLI" $ isLeft $ usageReport (UsageQuery Daily (Just "invalid") Nothing) daily
    assert "report rejects incompatible source period" $ isLeft $ usageReport allDaily monthly
    extractionTests
    typeTests
    fractionalTests
    let hostile = UsageReport (UsageQuery Daily (Just "\ESC[31m\x85\n|bad") Nothing) (UsageData Daily []) [UsagePoint 0 "\ESC[31m\x85\n|bad" 1]
        rendered = renderUsage hostile
    assert "terminal output sanitizes untrusted controls and injected table pipes" $ not ("\ESC" `T.isInfixOf` rendered) && not ("\x85" `T.isInfixOf` rendered) && not ("\n|bad" `T.isInfixOf` rendered)
    putStrLn "Usage extraction, exact numbers, duplicate periods, ranges and output checks passed."
  where
    assertEmpty label report = assert label $ null (usageEntries report) && field "total_bytes" (statistics report) == Number 0 && field "peak_bytes" (statistics report) == Null && field "peak_periods" (statistics report) == toJSON ([] :: [Text]) && field "aggregation_status" (statistics report) == String "complete"
    assertAmbiguous label report = assert label $ field "aggregation_status" (statistics report) == String "ambiguous" && field "total_bytes" (statistics report) == Null && field "peak_bytes" (statistics report) == Null && field "peak_periods" (statistics report) == toJSON ([] :: [Text]) && field "warnings" (usageJSON report) == toJSON (["duplicate_periods"] :: [Text])

extractionTests :: IO ()
extractionTests = do
    let json = "{\"category\":[\"2026-10-01\"],\"series\":[123.000]}"
        other = "{\"category\":[\"2026-10-02\"],\"series\":[999]}"
        decoys =
            script $
                T.unlines
                    [ "// var data = " <> other <> ";"
                    , "/* var data = " <> other <> "; */"
                    , "var quoted = 'var data = " <> other <> ";';"
                    , "var template = `var data = " <> other <> ";`;"
                    , "var unrelated = " <> other <> ";"
                    , "var metadata = {nested:{category:['wrong'],series:[999]}};"
                    ]
    fromDecoys <- right $ parse Daily $ decoys <> page json
    assert "comments strings template strings and other variable names cannot impersonate data" $ usagePoints fromDecoys == [UsagePoint 0 "2026-10-01" 123000]
    let unassociated = script $ "var data = " <> other <> ";"
    preferred <- right $ parse Daily $ unassociated <> page json
    assert "chart association selects correct script over schema-only decoy" $ usagePoints preferred == [UsagePoint 0 "2026-10-01" 123000]
    schemaOnly <- right $ parse Daily $ script $ "var data={\"unrelated\":true};" <> "var data=" <> json <> ";"
    assert "schema distinguishes unrelated data object without chart association" $ usagePoints schemaOnly == [UsagePoint 0 "2026-10-01" 123000]
    spaced <- right $ parse Daily $ script $ "const /* name */ data\n =\n" <> json <> ";" <> chart
    assert "whitespace comments and declaration variants accepted" $ usagePoints spaced == [UsagePoint 0 "2026-10-01" 123000]
    mapM_
        (\html -> assert "ambiguous or unrelated chart candidates rejected" $ isLeft $ parse Daily html)
        [ page json <> page other
        , page json <> page json
        , unassociated <> script ("var data=" <> json <> ";")
        , script $ "var data=" <> json <> ";var data=" <> other <> ";" <> chart
        , decoys
        , "<form id='loginForm'>登录</form>"
        , "<title>服务异常</title>"
        , script "var data={\"other\":[1,2]};"
        , script $ "var unrelated=" <> json <> ";" <> chart
        , script $ "var data={\"category\":[\"2026-10-01\"],\"series\":[1];" <> chart
        , script $ "var data={category:['2026-10-01'],series:[1]};" <> chart
        , script $ "var data=" <> json <> "||" <> other <> ";" <> chart
        , script $ "window.data=" <> json <> ";" <> chart
        , page "{\"category\":[],\"series\":null}" <> unassociated
        ]
    -- A function call is not JSON, even if evaluating it could return a valid object.
    assert "JavaScript expressions are not evaluated" $ isLeft $ parse Daily $ script $ "var data=(function(){throw 'never execute';return " <> json <> ";})();" <> chart
    bare <- right $ parse Daily $ script $ "data = " <> json <> ";" <> chart
    assert "standalone literal assignment recognized" $ usagePoints bare == [UsagePoint 0 "2026-10-01" 123000]
    let quotedAssociation = script $ "var data=" <> other <> ";var text='" <> T.replace "'" "\\'" chart <> "';"
    assert "chart references inside strings do not resolve ambiguity" $ isLeft $ parse Daily $ quotedAssociation <> script ("var data=" <> json <> ";")

typeTests :: IO ()
typeTests = do
    mapM_
        (\number -> assert "non-number and negative series rejected" $ isLeft $ parse Daily $ page $ "{\"category\":[\"2026-10-01\"],\"series\":[" <> number <> "]}")
        ["-1", "-0.1", "-0.00000000000000000001", "null", "\"123\"", "{}", "[]", "true"]
    mapM_
        (\json -> assert "invalid lengths arrays and categories rejected" $ isLeft $ parse Daily $ page json)
        [ "{\"category\":[\"2026-10-01\"],\"series\":[]}"
        , "{\"category\":[],\"series\":[1]}"
        , "{\"category\":[\"2026-02-29\"],\"series\":[1]}"
        , "{\"category\":[\"2026-10\"],\"series\":[1]}"
        , "{\"category\":[\"2026-1-01\"],\"series\":[1]}"
        , "{\"category\":[\"2026-10-01\\n\"],\"series\":[1]}"
        , "{\"category\":[null],\"series\":[1]}"
        , "{\"category\":[1],\"series\":[1]}"
        , "{\"category\":\"2026-10-01\",\"series\":[1]}"
        , "{\"category\":[\"2026-10-01\"],\"series\":1}"
        , "{\"category\":[],\"series\":null}"
        , "{\"category\":[]}"
        ]
    mapM_ (\key -> assert "invalid monthly category rejected" $ isLeft $ parse Monthly $ page $ "{\"category\":[\"" <> key <> "\"],\"series\":[1]}") ["2026-13", "2026-00", "2026-1", "2026-10-01"]
    exact <- right $ parse Daily $ page "{\"category\":[\"2026-10-01\",\"2026-10-02\",\"2026-10-03\"],\"series\":[123.000,123e2,9007199254742017.000]}"
    assert "integer-valued decimals exponents and large daily kB convert exactly" $ map pointBytes (usagePoints exact) == [123000, 12300000, 9007199254742017000]
    assert "invalid UTF-8 rejected" $ isLeft $ parseUsage Daily "\xff"

fractionalTests :: IO ()
fractionalTests = do
    html <- T.readFile "test/fixtures/usage-fractional.html"
    source <- right $ parse Daily html
    report <- right $ usageReport (UsageQuery Daily Nothing Nothing) source
    let json = usageJSON report
        stats = statistics report
        entries = array $ field "entries" json
        rendered = renderUsage report
    assert "daily fractional kB normalize to integer byte JSON without rounding" $
        map (field "bytes") entries == map Number [123456, 1024001, 1024001, 1]
            && eitherDecode (encode json) == Right json
    assert "normalized daily byte total and tied peaks remain exact" $
        field "total_bytes" stats == Number 2171459
            && field "peak_bytes" stats == Number 1024001
            && field "peak_periods" stats == toJSON (["2026-10-03", "2026-10-08"] :: [Text])
    assert "IEC display derives from normalized bytes, not chart kB" $
        all (`T.isInfixOf` rendered) ["120.56 KiB (123456)", "1000.00 KiB (1024001)", "1 B (1)", "2.07 MiB (2171459)"]
    selected <- right $ usageReport (UsageQuery Daily (Just "2026-10-03") (Just "2026-10-08")) source
    assert "normalized daily range keeps source indices and exact byte sum" $
        map sourceIndex (usageEntries selected) == [1, 2]
            && field "total_bytes" (statistics selected) == Number 2048002
    large <- right $ parse Daily $ page "{\"category\":[\"2026-10-01\",\"2026-10-02\"],\"series\":[9007199254742017.001,0.009]}"
    largeReport <- right $ usageReport (UsageQuery Daily Nothing Nothing) large
    assert "daily kB normalization never passes through binary floating point" $
        field "total_bytes" (statistics largeReport) == Number 9007199254742017010
            && field "peak_bytes" (statistics largeReport) == Number 9007199254742017001
    tinyReport <- right $ parse Daily (page "{\"category\":[\"2026-10-01\",\"2026-10-02\"],\"series\":[0.1,0.2]}") >>= usageReport (UsageQuery Daily Nothing Nothing)
    assert "decimal kB fractions yield exact integer byte sum" $ field "total_bytes" (statistics tinyReport) == Number 300
    duplicateSource <- right $ parse Daily $ page "{\"category\":[\"2026-10-01\",\"2026-10-01\"],\"series\":[123.456,123.456]}"
    duplicateReport <- right $ usageReport (UsageQuery Daily Nothing Nothing) duplicateSource
    assert "normalized duplicate periods remain ambiguous" $
        map pointBytes (usageEntries duplicateReport) == [123456, 123456]
            && field "aggregation_status" (statistics duplicateReport) == String "ambiguous"
            && field "total_bytes" (statistics duplicateReport) == Null
            && field "peak_bytes" (statistics duplicateReport) == Null
    monthly <- right $ parse Monthly $ page "{\"category\":[\"2026-10\"],\"series\":[2171459]}"
    monthlyReport <- right $ usageReport (UsageQuery Monthly Nothing Nothing) monthly
    assert "monthly values already bytes and never multiplied" $
        map pointBytes (usageEntries monthlyReport) == [2171459]
            && field "total_bytes" (statistics monthlyReport) == field "total_bytes" stats
    sameNumericDaily <- right $ parse Daily $ page "{\"category\":[\"2026-10-01\"],\"series\":[1]}"
    sameNumericMonthly <- right $ parse Monthly $ page "{\"category\":[\"2026-10\"],\"series\":[1]}"
    assert "source unit depends on endpoint, not whether JSON number has a fraction" $
        map pointBytes (usagePoints sameNumericDaily) == [1000] && map pointBytes (usagePoints sameNumericMonthly) == [1]
    mapM_ (\value -> assert "sub-byte daily results fail rather than round" $ isLeft $ parse Daily $ page $ "{\"category\":[\"2026-10-01\"],\"series\":[" <> value <> "]}") ["0.0001", "123.4567"]
    assert "fractional monthly bytes fail rather than infer kB" $ isLeft $ parse Monthly $ page "{\"category\":[\"2026-10\"],\"series\":[123.456]}"
