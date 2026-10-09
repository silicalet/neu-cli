{-# LANGUAGE OverloadedStrings #-}

module LogTests (logTests) where

import Control.Exception (try)
import Control.Monad (unless)
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM
import Data.Either (isLeft)
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import Data.Time (fromGregorian)
import NEU.HTTP.Session
import NEU.IPGW.Client (IPGWError)
import NEU.IPGW.Logs
import NEU.IPGW.SelfService
import NEU.IPGW.Traffic

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label
right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure
failure :: String -> IO a -> IO ()
failure label action = do
    result <- try (action >> pure ()) :: IO (Either IPGWError ())
    assert label $ isLeft result

mock :: [Reply] -> IO (SelfService, IO [RequestSpec])
mock replies = do
    remaining <- newIORef replies
    seen <- newIORef []
    let send spec = do
            modifyIORef' seen (++ [spec])
            responses <- readIORef remaining
            case responses of
                [] -> fail "unexpected log request"
                r : rs -> writeIORef remaining rs >> pure r
        finish = do
            rest <- readIORef remaining
            assert "all log requests used" $ null rest
            readIORef seen
    pure (selfServiceWithSession $ sessionWithTransport send, finish)

logTests :: IO ()
logTests = do
    traffic <- T.readFile "test/fixtures/traffic-log.html"
    billing <- T.readFile "test/fixtures/billing-log.html"
    page <- right $ parseLogPage TrafficLog $ TE.encodeUtf8 traffic
    billPage <- right $ parseLogPage BillingLog $ TE.encodeUtf8 billing
    let rows = pageEntries page
        bills = pageEntries billPage
    assert "hidden bytes and seconds plus IPv6 preserved" $ case rows of
        [a@TrafficEntry {}, b@TrafficEntry {}] -> bytes a == 1073741825 && seconds a == 61 && address a == "2001:db8::1" && productName a == "测试 & 套餐" && bytes b == 1024
        _ -> False
    assert "exact scientific amounts" $ field "cost_yuan" (statistics TrafficLog rows) == Number 0.30
    assert "billing costs kept separate incl negative adjustment" $ field "cost_yuan" (statistics BillingLog bills) == Number 10 && field "realtime_cost_yuan" (statistics BillingLog bills) == Number 0.30
    assert "large byte counts remain exact" $ field "bytes" (statistics BillingLog bills) == Number 9007199254742017
    assert "human and exact units rendered" $ "1.00 GiB (1073741825)" `T.isInfixOf` renderLogs TrafficLog rows
    assert "balance preserves four decimals" $ "10.1234" `T.isInfixOf` renderLogs TrafficLog rows
    mapM_
        (\body -> assert "reject invalid or expired log page" $ isLeft $ parseLogPage TrafficLog $ TE.encodeUtf8 body)
        [ "<title>登录</title><form id='loginForm'></form>"
        , T.replace "data-col-seq=\"16\"" "data-col-seq=\"99\"" traffic
        , T.replace ">1073741825<" ">1.00G<" traffic
        , T.replace ">1073741825<" ">-1<" traffic
        , T.replace ">0.10<" ">unknown<" traffic
        , T.replace "2026-10-01 10:01:01" "invalid" traffic
        , T.replace "第<b>1-2</b>条，共<b>2</b>条数据." "" traffic
        ]
    let empty =
            withRows traffic "<tr><td colspan=\"11\"><div class=\"empty\">没有找到数据。</div></td></tr>"
                & T.replace "<div class=\"summary\">第<b>1-2</b>条，共<b>2</b>条数据.</div>" ""
    emptyPage <- right $ parseLogPage TrafficLog $ TE.encodeUtf8 empty
    assert "real empty table accepted" $ totalRecords emptyPage == 0 && null (pageEntries emptyPage)
    let allQuery = LogQuery Nothing Nothing Nothing noTrafficFilter
        reply body = Reply "https://ipgw.neu.edu.cn:8800/log/detail" $ TE.encodeUtf8 body
        firstRow = T.takeWhile (/= '\n') $ T.drop 1 $ snd $ T.breakOn "\n<tr data-key=" traffic
        firstTen =
            withRows traffic (T.concat [T.replace "traffic-1" ("multi-" <> T.pack (show i)) firstRow | i <- [1 .. 10 :: Int]])
                & T.replace "第<b>1-2</b>条，共<b>2</b>条数据." "第<b>1-10</b>条，共<b>12</b>条数据."
        lastTwo = T.replace "第<b>1-2</b>条，共<b>2</b>条数据." "第<b>11-12</b>条，共<b>12</b>条数据." traffic
    (multi, finishMulti) <- mock [reply firstTen, reply lastTwo]
    report <- fetchLogs multi TrafficLog allQuery
    requests <- finishMulti
    assert "all pages fetched and summed" $ fetchedRecords report == 12 && length (reportEntries report) == 12 && length requests == 2 && "page=2&per-page=10" `T.isInfixOf` requestURL (requests !! 1)
    (single, finishSingle) <- mock [reply lastTwo]
    onePage <- fetchLogs single TrafficLog $ LogQuery (Just 2) Nothing Nothing noTrafficFilter
    _ <- finishSingle
    assert "single-page scope explicit" $ fetchedRecords onePage == 2 && sourceTotal onePage == 12 && field "scope" (reportJSON onePage) == String "page"
    (filtered, finishFiltered) <- mock [reply traffic]
    let day = fromGregorian 2026 10 2
    selected <- fetchLogs filtered TrafficLog $ LogQuery Nothing (Just day) (Just day) noTrafficFilter
    _ <- finishFiltered
    assert "inclusive local date filter" $ map entryID (reportEntries selected) == ["traffic-2"] && fetchedRecords selected == 2
    let ipFilter = noTrafficFilter {filterIPs = ["192.0.2.1"], filterProducts = ["测试套餐"], filterMinBytes = Just 1024, filterMaxSeconds = Just 120}
        ipQuery = allQuery {queryTraffic = ipFilter}
    (byIP, finishIP) <- mock [reply firstTen, reply lastTwo]
    ipReport <- fetchLogs byIP TrafficLog ipQuery
    _ <- finishIP
    let grouped = ipUsageJSON ipReport
    assert "IP filters apply after all pages even when first page has no match" $
        sourceTotal ipReport == 12 && fetchedRecords ipReport == 12 && map entryID (reportEntries ipReport) == ["traffic-2"]
    assert "grouped usage statistics cover selected sessions, not source pages" $
        field "bytes" (field "statistics" grouped) == Number 1024
            && field "seconds" (field "statistics" grouped) == Number 120
            && field "ip_count" grouped == Number 1
            && field "filters" grouped == trafficFilterJSON ipFilter
    (hiddenDuplicate, finishHiddenDuplicate) <- mock [reply firstTen, reply $ T.replace "traffic-1" "multi-1" lastTwo]
    failure "filtering cannot hide duplicate page records" $ fetchLogs hiddenDuplicate TrafficLog ipQuery
    _ <- finishHiddenDuplicate
    let crossesMidnight = T.replace "2026-10-01 10:01:01" "2026-10-02 00:01:01" traffic
    (crossDay, finishCrossDay) <- mock [reply crossesMidnight]
    crossReport <- fetchLogs crossDay TrafficLog $ allQuery {queryFrom = Just day, queryTo = Just day, queryTraffic = noTrafficFilter {filterIPs = ["2001:db8::1"]}}
    _ <- finishCrossDay
    assert "date filtering uses session start, never prorates cross-midnight traffic" $
        null (reportEntries crossReport) && field "bytes" (field "statistics" $ ipUsageJSON crossReport) == Number 0
    (invalidFilters, finishInvalidFilters) <- mock []
    failure "inverted byte bounds rejected before requesting logs" $
        fetchLogs invalidFilters TrafficLog $
            allQuery {queryTraffic = noTrafficFilter {filterMinBytes = Just 2, filterMaxBytes = Just 1}}
    failure "device fields cannot silently filter billing records" $ fetchLogs invalidFilters BillingLog ipQuery
    _ <- finishInvalidFilters
    (zero, finishZero) <- mock [reply empty]
    noRows <- fetchLogs zero TrafficLog allQuery
    _ <- finishZero
    assert "empty output has zero totals" $ field "bytes" (statistics TrafficLog $ reportEntries noRows) == Number 0
    (duplicate, finishDuplicate) <- mock [reply firstTen, reply $ T.replace "traffic-1" "multi-1" lastTwo]
    failure "duplicate page records cannot double-count" $ fetchLogs duplicate TrafficLog allQuery
    _ <- finishDuplicate
    (changed, finishChanged) <- mock [reply firstTen, reply $ T.replace "<b>12</b>" "<b>13</b>" lastTwo]
    failure "changing source total rejected" $ fetchLogs changed TrafficLog allQuery
    _ <- finishChanged
    (ignored, finishIgnored) <- mock [reply traffic]
    failure "server ignoring page argument rejected" $ fetchLogs ignored TrafficLog $ LogQuery (Just 2) Nothing Nothing noTrafficFilter
    _ <- finishIgnored
    let home = Reply "https://ipgw.neu.edu.cn:8800/home" "<a href=\"/log/detail\">log</a><a href=\"/log/check-out\">billing</a>"
    (auth, finishAuth) <- mock [home, home]
    authenticateSelfService auth "fixture-user" "fixture-secret"
    authRequests <- finishAuth
    assert "self-service login never activates gateway" $ all (not . T.isInfixOf "/v1/srun_portal_sso" . requestURL) authRequests
    (invalidAuth, finishInvalidAuth) <- mock [home, home {replyBody = "<title>服务异常</title>"}]
    failure "HTTP success alone is not authenticated home" $ authenticateSelfService invalidAuth "fixture-user" "fixture-secret"
    _ <- finishInvalidAuth
    putStrLn "Log parsing, exact statistics, pagination, filters and self-service authentication checks passed."
  where
    field key (Object fields) = maybe Null id $ KM.lookup key fields
    field _ _ = Null
    (&) = flip ($)
    withRows html rows =
        let (before, after) = T.breakOn "<tbody>" html
            (_, ending) = T.breakOn "</tbody>" after
         in before <> "<tbody>" <> rows <> ending
