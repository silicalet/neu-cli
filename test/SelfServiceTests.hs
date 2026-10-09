{-# LANGUAGE OverloadedStrings #-}

module SelfServiceTests (selfServiceTests) where

import Control.Exception (try)
import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key (Key)
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isControl)
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import Data.Time (fromGregorian)
import NEU.HTTP.Session
import NEU.IPGW.Client (IPGWError)
import NEU.IPGW.Logs
import NEU.IPGW.Overview qualified as O
import NEU.IPGW.SelfService
import NEU.IPGW.Traffic (noTrafficFilter)
import NEU.IPGW.Usage qualified as U

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

failure :: String -> IO a -> IO ()
failure label action = do
    result <- try (action >> pure ()) :: IO (Either IPGWError ())
    assert label $ isLeft result

field :: Key -> Value -> Value
field key (Object fields) = maybe Null id $ KM.lookup key fields
field _ _ = Null

mock :: [Reply] -> IO (SelfService, IO [RequestSpec])
mock replies = do
    remaining <- newIORef replies
    seen <- newIORef []
    let send spec = do
            modifyIORef' seen (spec :)
            responses <- readIORef remaining
            case responses of
                [] -> fail "unexpected self-service request"
                r : rs -> writeIORef remaining rs >> pure r
        finish = do
            rest <- readIORef remaining
            assert "all expected self-service replies consumed" $ null rest
            reverse <$> readIORef seen
    pure (selfServiceWithSession $ sessionWithTransport send, finish)

reply :: Text -> Text -> Reply
reply path = Reply ("https://ipgw.neu.edu.cn:8800" <> path) . TE.encodeUtf8

withRows :: Text -> Text -> Text
withRows html rows =
    let (before, after) = T.breakOn "<tbody>" html
        (_, ending) = T.breakOn "</tbody>" after
     in before <> "<tbody>" <> rows <> ending

selfServiceTests :: IO ()
selfServiceTests = do
    payment <- T.readFile "test/fixtures/payment-log.html"
    empty <- T.readFile "test/fixtures/payment-empty.html"
    parsed <- right $ parseLogPage PaymentLog $ TE.encodeUtf8 payment
    let rows = pageEntries parsed
        stats = statistics PaymentLog rows
        allQuery = LogQuery Nothing Nothing Nothing noTrafficFilter
        groups =
            toJSON
                [ object ["payment_type" .= ("A & 新类型" :: Text), "records" .= (2 :: Int), "amount_yuan" .= (10 :: Int)]
                , object ["payment_type" .= ("B 类型" :: Text), "records" .= (2 :: Int), "amount_yuan" .= Number 0.0001]
                ]
    assert "payment amounts and unknown types preserved" $
        map entryJSON rows
            == [ object ["id" .= ("pay-1" :: Text), "username" .= ("fixture-user" :: Text), "paid_at" .= ("2026-10-01 00:00:00" :: Text), "amount_yuan" .= Number 10.1234, "payment_type" .= ("A & 新类型" :: Text)]
               , object ["id" .= ("pay-2" :: Text), "username" .= ("fixture-user" :: Text), "paid_at" .= ("2026-10-02 23:59:59" :: Text), "amount_yuan" .= Number (-0.1234), "payment_type" .= ("A & 新类型" :: Text)]
               , object ["id" .= ("pay-3" :: Text), "username" .= ("fixture-user" :: Text), "paid_at" .= ("2026-10-03 00:00:00" :: Text), "amount_yuan" .= Number 0, "payment_type" .= ("B 类型" :: Text)]
               , object ["id" .= ("pay-4" :: Text), "username" .= ("fixture-user" :: Text), "paid_at" .= ("2026-10-04 12:00:00" :: Text), "amount_yuan" .= Number 0.0001, "payment_type" .= ("B 类型" :: Text)]
               ]
    assert "payment exact algebraic totals and sorted types" $ stats == object ["records" .= (4 :: Int), "amount_yuan" .= Number 10.0001, "by_type" .= groups]
    assert "payment has no invented counters" $ map entryBytes rows == replicate 4 Nothing
    forM_ [T.replace "data-col-seq=\"6\"" "data-col-seq=\"5\"" payment, T.replace ">10.1234<" ">unknown<" payment, T.replace ">pay-1<" ">mismatch<" payment, T.replace "2026-10-01 00:00:00" "invalid" payment] $ \body ->
        assert "malformed payment fields rejected" $ isLeft $ parseLogPage PaymentLog $ TE.encodeUtf8 body
    let hostile = T.replace "A &amp; 新类型" "A\ESC[31m\x009b|新类型" payment
    hostilePage <- right $ parseLogPage PaymentLog $ TE.encodeUtf8 hostile
    assert "payment terminal fields cannot emit controls" $ all (\c -> not (isControl c) || c == '\n') $ T.unpack $ renderLogs PaymentLog $ pageEntries hostilePage
    (zero, finishZero) <- mock [reply "/log/pay" empty]
    noRows <- fetchLogs zero PaymentLog allQuery
    _ <- finishZero
    assert "empty payment has zero money and no fake usage" $ statistics PaymentLog (reportEntries noRows) == object ["records" .= (0 :: Int), "amount_yuan" .= (0 :: Int), "by_type" .= ([] :: [Value])]
    (filtered, finishFiltered) <- mock [reply "/log/pay" payment]
    selected <- fetchLogs filtered PaymentLog $ LogQuery Nothing (Just $ fromGregorian 2026 10 1) (Just $ fromGregorian 2026 10 2) noTrafficFilter
    _ <- finishFiltered
    assert "payment dates include entire boundary days" $ map entryID (reportEntries selected) == ["pay-1", "pay-2"] && fetchedRecords selected == 4
    assert "payment report date basis and filtered statistics" $ field "date_basis" (reportJSON selected) == String "paid_at" && field "amount_yuan" (field "statistics" $ reportJSON selected) == Number 10
    let firstRow = T.takeWhile (/= '\n') $ T.drop 1 $ snd $ T.breakOn "\n<tr data-key=" payment
        firstTen =
            T.replace "第<b>1-4</b>条，共<b>4</b>条数据." "第<b>1-10</b>条，共<b>14</b>条数据." $
                withRows payment $
                    T.concat [T.replace "pay-1" ("multi-" <> T.pack (show n)) firstRow | n <- [1 .. 10 :: Int]]
        lastFour = T.replace "第<b>1-4</b>条，共<b>4</b>条数据." "第<b>11-14</b>条，共<b>14</b>条数据." payment
    (multi, finishMulti) <- mock [reply "/log/pay" firstTen, reply "/log/pay" lastFour]
    report <- fetchLogs multi PaymentLog allQuery
    requests <- finishMulti
    assert "payment pagination exact result and GET query" $ fetchedRecords report == 14 && field "amount_yuan" (statistics PaymentLog $ reportEntries report) == Number 111.2341 && map requestURL requests == ["https://ipgw.neu.edu.cn:8800/log/pay?page=1&per-page=10", "https://ipgw.neu.edu.cn:8800/log/pay?page=2&per-page=10"] && all ((== Nothing) . requestForm) requests
    (single, finishSingle) <- mock [reply "/log/pay" lastFour]
    onePage <- fetchLogs single PaymentLog $ LogQuery (Just 2) Nothing Nothing noTrafficFilter
    _ <- finishSingle
    assert "payment single-page scope" $ fetchedRecords onePage == 4 && sourceTotal onePage == 14 && field "scope" (reportJSON onePage) == String "page"
    forM_
        [ [firstTen, T.replace "pay-1" "multi-1" lastFour]
        , [firstTen, T.replace "<b>14</b>" "<b>15</b>" lastFour]
        , [firstTen, payment]
        , [T.replace "pay-2" "pay-1" payment]
        , [T.replace "<b>14</b>" "<b>10001</b>" firstTen]
        ]
        $ \pages -> do
            (bad, finishBad) <- mock $ map (reply "/log/pay") pages
            failure "payment pagination inconsistencies fail without report" $ fetchLogs bad PaymentLog allQuery
            _ <- finishBad
            pure ()
    (invalidQuery, finishInvalid) <- mock []
    failure "invalid payment query performs no requests" $ fetchLogs invalidQuery PaymentLog $ LogQuery Nothing (Just $ fromGregorian 2026 10 2) (Just $ fromGregorian 2026 10 1) noTrafficFilter
    _ <- finishInvalid
    workflowTests payment
    putStrLn "Payment precision, paging, filters and read-only self-service workflows passed."

workflowTests :: Text -> IO ()
workflowTests payment = do
    home <- T.readFile "test/fixtures/overview.html"
    daily <- T.readFile "test/fixtures/usage-fractional.html"
    monthly <- T.readFile "test/fixtures/usage-monthly.html"
    login <- T.readFile "test/fixtures/login.html"
    key <- T.strip <$> T.readFile "test/fixtures/public-key.b64"
    let landing = reply "/home" home
        authReplies = [Reply "https://pass.neu.edu.cn/tpass/login" (TE.encodeUtf8 login), Reply "https://pass.neu.edu.cn/tpass/login_neu.js" (TE.encodeUtf8 $ "var publicKeyStr = \"" <> key <> "\";"), landing, landing]
    (service, finish) <- mock $ authReplies ++ [landing, reply "/statistic/index" daily, reply "/statistic/flow" monthly, reply "/log/pay" payment]
    authenticateSelfService service "fixture-user" "fixture-secret"
    overview <- fetchOverview service
    assert "authenticated overview preserves identity and exact product money" $ O.accountUsername (O.overviewAccount overview) == "fixture-user" && map O.productBalance (O.overviewProducts overview) == [Just 10.9876] && map O.deviceIP (O.overviewDevices overview) == ["192.0.2.1"]
    dailyReport <- fetchUsage service $ U.UsageQuery U.Daily (Just "2026-10-03") (Just "2026-10-08")
    assert "authenticated daily kB filter retains indices and exact byte totals" $ map U.sourceIndex (U.usageEntries dailyReport) == [1, 2] && field "total_bytes" (field "statistics" $ U.usageJSON dailyReport) == Number 2048002
    monthlyReport <- fetchUsage service $ U.UsageQuery U.Monthly Nothing Nothing
    assert "authenticated monthly duplicates prohibit accidental summation" $ map U.pointBytes (U.usageEntries monthlyReport) == [1024, 2048, 4096, 8192, 8192, 4096] && field "total_bytes" (field "statistics" $ U.usageJSON monthlyReport) == Null && field "aggregation_status" (field "statistics" $ U.usageJSON monthlyReport) == String "ambiguous"
    payments <- fetchLogs service PaymentLog $ LogQuery Nothing Nothing Nothing noTrafficFilter
    assert "authenticated payment workflow produces exact balance of entries" $ field "amount_yuan" (statistics PaymentLog $ reportEntries payments) == Number 10.0001
    requests <- finish
    assert "one CAS authentication form for all same-session reads" $ length [() | spec <- requests, requestForm spec /= Nothing] == 1
    assert "all business requests are read-only and use only confirmed routes" $ map requestURL (drop 3 requests) == map ("https://ipgw.neu.edu.cn:8800" <>) ["/home", "/home", "/statistic/index", "/statistic/flow", "/log/pay?page=1&per-page=10"] && all ((== Nothing) . requestForm) (drop 3 requests)
    forM_ ["<title>服务异常</title>", login] $ \body -> do
        (expired, finishExpired) <- mock [reply "/home" body]
        failure "expired overview cannot masquerade as zero online devices" $ fetchOverview expired
        _ <- finishExpired
        pure ()
    (invalid, finishInvalid) <- mock []
    failure "invalid usage query does not touch network" $ fetchUsage invalid $ U.UsageQuery U.Monthly (Just "2026-13") Nothing
    _ <- finishInvalid
    pure ()
