{-# LANGUAGE OverloadedStrings #-}

module TrafficTests (trafficTests) where

import Control.Monad (unless)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isControl)
import Data.Either (isLeft)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import NEU.IPGW.Logs (LogEntry (..), humanBytes)
import NEU.IPGW.Traffic

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

field :: Key -> Value -> Value
field key (Object fields) = maybe Null id $ KM.lookup key fields
field _ _ = Null

at :: Integer -> Int -> Int -> LocalTime
at day hour minute = LocalTime (fromGregorian 2026 10 $ fromInteger day) (TimeOfDay hour minute 0)

entry :: Text -> Text -> Text -> Integer -> Integer -> Scientific -> LocalTime -> LocalTime -> LogEntry
entry identifier ip plan b s c start end = TrafficEntry identifier "account" start end ip plan 0 b s c

trafficTests :: IO ()
trafficTests = do
    let a = entry "a" "2001:db8::1" "alpha" 10 60 0.1 (at 1 23 59) (at 2 0 0)
        b = entry "b" "2001:db8::1" "beta" 20 120 0.2 (at 2 1 0) (at 2 1 2)
        c = entry "c" "10.0.0.2" "alpha" 30 180 0.0001 (at 2 2 0) (at 2 2 3)
        d = entry "d" "2001:db8::10" "beta" 40 240 0.4 (at 2 3 0) (at 2 3 4)
        rows = [a, b, c, d]
        ids f = map recordID $ filter (matchesTrafficFilter f) rows
        billing = BillingEntry "bill" "account" (at 2 0 0) 10 1 100 1
        payment = PaymentEntry "payment" "account" (at 2 0 0) 20 "online"
    assert "inactive filter accepts every traffic record" $ ids noTrafficFilter == ["a", "b", "c", "d"]
    assert "nontraffic records never match or group" $
        not (matchesTrafficFilter noTrafficFilter billing)
            && not (matchesTrafficFilter noTrafficFilter payment)
            && null (trafficGroups [billing, payment])
    assert "repeated IP values are OR, not intersections or extra records" $
        ids noTrafficFilter {filterIPs = ["10.0.0.2", "2001:db8::1", "2001:db8::1"]} == ["a", "b", "c"]
    assert "repeated products are OR" $
        ids noTrafficFilter {filterProducts = ["beta", "alpha", "alpha"]} == ["a", "b", "c", "d"]
    let constrained = noTrafficFilter {filterIPs = ["2001:db8::1", "10.0.0.2"], filterProducts = ["alpha"], filterMinBytes = Just 10, filterMaxBytes = Just 30, filterMinSeconds = Just 60, filterMaxSeconds = Just 180}
    assert "dimensions AND and endpoints inclusive" $ ids constrained == ["a", "c"]
    assert "equal byte and duration endpoints allowed" $
        validateTrafficFilter noTrafficFilter {filterMinBytes = Just 10, filterMaxBytes = Just 10, filterMinSeconds = Just 60, filterMaxSeconds = Just 60} == Right ()
            && ids noTrafficFilter {filterMinBytes = Just 10, filterMaxBytes = Just 10, filterMinSeconds = Just 60, filterMaxSeconds = Just 60} == ["a"]
    assert "all four range bounds exclude outlying records" $
        ids noTrafficFilter {filterMinBytes = Just 11} == ["b", "c", "d"]
            && ids noTrafficFilter {filterMaxBytes = Just 29} == ["a", "b"]
            && ids noTrafficFilter {filterMinSeconds = Just 61} == ["b", "c", "d"]
            && ids noTrafficFilter {filterMaxSeconds = Just 179} == ["a", "b"]
    assert "IPv6 matching is exact without substring or canonicalization" $
        ids noTrafficFilter {filterIPs = ["2001:db8::1"]} == ["a", "b"]
            && null (ids noTrafficFilter {filterIPs = ["2001:0db8::1"]})
            && null (ids noTrafficFilter {filterIPs = ["2001:DB8::1"]})
    assert "nonblank raw filter strings are neither trimmed nor normalized" $
        validateTrafficFilter noTrafficFilter {filterProducts = [" alpha "]} == Right ()
            && null (ids noTrafficFilter {filterProducts = [" alpha "]})
    mapM_
        (\f -> assert "reject invalid filter values" $ isLeft $ validateTrafficFilter f)
        [ noTrafficFilter {filterMinBytes = Just (-1)}
        , noTrafficFilter {filterMaxBytes = Just (-1)}
        , noTrafficFilter {filterMinSeconds = Just (-1)}
        , noTrafficFilter {filterMaxSeconds = Just (-1)}
        , noTrafficFilter {filterMinBytes = Just 2, filterMaxBytes = Just 1}
        , noTrafficFilter {filterMinSeconds = Just 2, filterMaxSeconds = Just 1}
        , noTrafficFilter {filterIPs = [""]}
        , noTrafficFilter {filterIPs = ["valid", " \t\n"]}
        , noTrafficFilter {filterProducts = [""]}
        , noTrafficFilter {filterProducts = ["valid", " \t\n"]}
        ]
    assert "zero bounds and inactive filter validate" $
        validateTrafficFilter noTrafficFilter == Right ()
            && validateTrafficFilter noTrafficFilter {filterMinBytes = Just 0, filterMaxBytes = Just 0, filterMinSeconds = Just 0, filterMaxSeconds = Just 0} == Right ()
    let grouped = trafficGroups [d, b, payment, c, a, billing]
        json = map trafficGroupJSON grouped
        sameIP = trafficGroups [b, a]
    assert "groups are in lexical raw IP order" $ map (field "ip") json == map String ["10.0.0.2", "2001:db8::1", "2001:db8::10"]
    assert "same IP across products combines exact counters, costs and time envelope" $
        map trafficGroupJSON sameIP
            == [ object
                    [ "ip" .= ("2001:db8::1" :: Text)
                    , "products" .= (["alpha", "beta"] :: [Text])
                    , "records" .= (2 :: Integer)
                    , "bytes" .= (30 :: Integer)
                    , "seconds" .= (180 :: Integer)
                    , "cost_yuan" .= (0.3 :: Scientific)
                    , "first_started_at" .= ("2026-10-01 23:59:00" :: Text)
                    , "last_ended_at" .= ("2026-10-02 01:02:00" :: Text)
                    ]
               ]
    assert "aggregation is independent of input order" $ grouped == trafficGroups [a, c, b, d]
    assert "products are unique" $
        map (field "products" . trafficGroupJSON) (trafficGroups [a, a, b]) == [toJSON (["alpha", "beta"] :: [Text])]
    let selected = trafficGroups $ filter (matchesTrafficFilter noTrafficFilter {filterMaxBytes = Just 10}) [a, b]
    assert "per-record filtering precedes aggregation" $
        map (field "bytes" . trafficGroupJSON) selected == [Number 10]
            && map (field "records" . trafficGroupJSON) selected == [Number 1]
            && map (field "products" . trafficGroupJSON) selected == [toJSON (["alpha"] :: [Text])]
    let huge = 9007199254742017
        big = entry "big" "big-ip" "big-product" huge huge 0.000000000000000001 (at 1 0 0) (at 3 0 0)
        bigGroups = trafficGroups [big, big]
    assert "large integer counters and tiny scientific costs stay exact" $
        map (field "bytes" . trafficGroupJSON) bigGroups == [Number $ fromInteger $ 2 * huge]
            && map (field "seconds" . trafficGroupJSON) bigGroups == [Number $ fromInteger $ 2 * huge]
            && map (field "cost_yuan" . trafficGroupJSON) bigGroups == [Number 0.000000000000000002]
    assert "text keeps IEC and exact byte counts and full cost precision" $
        (humanBytes (2 * huge) <> " (" <> T.pack (show $ 2 * huge) <> ")") `T.isInfixOf` renderTrafficGroups bigGroups
            && "0.000000000000000002" `T.isInfixOf` renderTrafficGroups bigGroups
    assert "empty groups and zero totals render" $
        null (trafficGroups [])
            && all (`T.isInfixOf` renderTrafficGroups []) ["IP 数：0", "记录数：0", "0 B (0)", "0 秒", "0.00 元"]
    let spanning = entry "spanning" "2001:db8::1" "alpha" 1 1 0 (at 1 22 0) (at 3 1 0)
        envelope = map trafficGroupJSON $ trafficGroups [b, spanning, a]
    assert "time envelope uses independent start minimum and end maximum" $
        map (field "first_started_at") envelope == [String "2026-10-01 22:00:00"]
            && map (field "last_ended_at") envelope == [String "2026-10-03 01:00:00"]
    assert "text totals aggregate all groups" $
        all (`T.isInfixOf` renderTrafficGroups grouped) ["IP 数：3", "记录数：4", "100 B (100)", "600 秒", "0.7001 元"]
    let hostile = "bad\ESC[31m\n\t|value"
        unsafeEntry = entry "unsafe" hostile hostile 0 0 0 (at 1 0 0) (at 1 0 0)
        unsafeGroups = trafficGroups [unsafeEntry]
        displayedFilter = renderTrafficFilter noTrafficFilter {filterIPs = [hostile], filterProducts = [hostile]}
    assert "terminal controls and delimiters cannot escape filter lines" $
        length displayedFilter == 2 && all (T.all (\ch -> not (isControl ch) && ch /= '|')) displayedFilter
    assert "terminal controls cannot escape table rows" $
        T.all (\ch -> ch == '\n' || not (isControl ch)) (renderTrafficGroups unsafeGroups)
            && length (T.lines $ renderTrafficGroups unsafeGroups) == length (T.lines $ renderTrafficGroups $ trafficGroups [a])
            && not ("|value" `T.isInfixOf` renderTrafficGroups unsafeGroups)
    assert "JSON preserves raw keys while encoding controls safely" $
        map (field "ip" . trafficGroupJSON) unsafeGroups == [String hostile]
            && field "ips" (trafficFilterJSON noTrafficFilter {filterIPs = [hostile]}) == toJSON [hostile]
    putStrLn "Traffic filters and exact per-IP aggregation checks passed."
