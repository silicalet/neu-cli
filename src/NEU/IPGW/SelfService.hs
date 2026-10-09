{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.SelfService (
    SelfService,
    newSelfService,
    selfServiceWithSession,
    authenticateSelfService,
    LogQuery (..),
    validateLogQuery,
    LogReport (..),
    fetchLogs,
    fetchOverview,
    fetchUsage,
    reportJSON,
    renderReport,
    ipUsageJSON,
    renderIPUsage,
) where

import Control.Exception (catch, throwIO)
import Control.Monad (unless, when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (Day, localDay)
import NEU.Auth.CAS qualified as CAS
import NEU.HTTP.Session
import NEU.HTTP.URL (urlWith)
import NEU.IPGW.Client (IPGWError (..))
import NEU.IPGW.Logs
import NEU.IPGW.Overview (Overview, parseOverview)
import NEU.IPGW.Traffic
import NEU.IPGW.Usage
import Text.HTML.TagSoup (Tag (..), parseTags)

newtype SelfService = SelfService Session

data LogQuery = LogQuery
    { queryPage :: Maybe Int
    , queryFrom :: Maybe Day
    , queryTo :: Maybe Day
    , queryTraffic :: TrafficFilter
    }
    deriving (Eq, Show)
data LogReport = LogReport
    { reportKind :: LogKind
    , reportQuery :: LogQuery
    , sourceTotal :: Int
    , fetchedRecords :: Int
    , reportEntries :: [LogEntry]
    }
    deriving (Eq, Show)

baseURL :: Text
baseURL = "https://ipgw.neu.edu.cn:8800"

selfServiceWithSession :: Session -> SelfService
selfServiceWithSession =
    SelfService
        . withRedirectRules
            [ RedirectRule (base <> "/sso/neusoft/index") "http://selfservice/home" (baseURL <> "/home")
            | base <- ["http://ipgw.neu.edu.cn:8800", baseURL]
            ]

newSelfService :: Bool -> Int -> IO SelfService
newSelfService bypass timeout = wrapErrors $ selfServiceWithSession <$> newSession bypass timeout

service :: IO CAS.Service
service = checked $ CAS.mkService "http://ipgw.neu.edu.cn:8800/sso/neusoft/index" ["http://ipgw.neu.edu.cn:8800", baseURL]

wrapErrors :: IO a -> IO a
wrapErrors action =
    action
        `catch` (\(ServiceError message) -> throwIO $ IPGWError message)
        `catch` (\err -> throwIO $ IPGWError $ CAS.casErrorMessage err)
checked :: Either Text a -> IO a
checked = either (throwIO . IPGWError) pure

authenticateSelfService :: SelfService -> Text -> Text -> IO ()
authenticateSelfService (SelfService session) username password = wrapErrors $ do
    target <- service
    _ <- CAS.authenticate session target username password
    -- Check an authenticated-only page, not just a successful HTTP status.
    home <- CAS.requestService session target $ RequestSpec (baseURL <> "/home") Nothing Nothing
    html <- checked $ either (const $ Left "自助服务首页编码无效") Right $ TE.decodeUtf8' $ replyBody home
    let links = [href | TagOpen "a" attrs <- parseTags html, Just href <- [lookup "href" attrs]]
    unless (all (`elem` links) ["/log/detail", "/log/check-out"]) $
        throwIO $
            IPGWError "未能确认自助服务登录成功；可能需要浏览器验证"
fetchOverview :: SelfService -> IO Overview
fetchOverview (SelfService session) = wrapErrors $ do
    target <- service
    reply <- CAS.requestService session target $ RequestSpec (baseURL <> "/home") Nothing Nothing
    checked $ parseOverview $ replyBody reply

fetchUsage :: SelfService -> UsageQuery -> IO UsageReport
fetchUsage (SelfService session) query = wrapErrors $ do
    checked $ validateUsageQuery query
    target <- service
    reply <- CAS.requestService session target $ RequestSpec (baseURL <> usagePath (usagePeriod query)) Nothing Nothing
    parsed <- checked $ parseUsage (usagePeriod query) $ replyBody reply
    checked $ usageReport query parsed

validateLogQuery :: LogKind -> LogQuery -> Either Text ()
validateLogQuery kind query = do
    when (maybe False (\n -> n < 1 || n > 1000000) $ queryPage query) $ Left "页码必须为 1–1000000 的整数"
    when (case (queryFrom query, queryTo query) of (Just a, Just b) -> a > b; _ -> False) $
        Left "起始日期不能晚于结束日期"
    validateTrafficFilter $ queryTraffic query
    unless (kind == TrafficLog || queryTraffic query == noTrafficFilter) $
        Left "IP、产品、流量与时长筛选仅适用于 traffic-log 或 usage --by-ip"

fetchLogs :: SelfService -> LogKind -> LogQuery -> IO LogReport
fetchLogs (SelfService session) kind query = wrapErrors $ do
    checked $ validateLogQuery kind query
    target <- service
    let getPage page = do
            reply <-
                CAS.requestService session target $
                    RequestSpec
                        (urlWith (baseURL <> logPath kind) [("page", TE.encodeUtf8 $ T.pack $ show page), ("per-page", "10")])
                        Nothing
                        Nothing
            parsed <- checked $ parseLogPage kind $ replyBody reply
            let total = totalRecords parsed
                expectedFirst = (page - 1) * 10 + 1
                expectedLast = min (page * 10) total
            unless ((total == 0 && page == 1) || (firstRecord parsed == expectedFirst && lastRecord parsed == expectedLast && total >= expectedFirst)) $
                throwIO $
                    IPGWError "日志页码无效或服务器未按请求分页"
            pure parsed
        collect total page seen acc
            | page > total `div` 10 + (if total `mod` 10 == 0 then 0 else 1) = pure $ concat $ reverse acc
            | page > 1000 = throwIO $ IPGWError "日志超过 1000 页，请用 --page 分页读取"
            | otherwise = do
                next <- getPage page
                unless (totalRecords next == total) $ throwIO $ IPGWError "查询期间日志总数发生变化，请重试"
                let rows = pageEntries next
                seen' <- unique seen rows
                collect total (page + 1) seen' (rows : acc)
    let page = maybe 1 id $ queryPage query
    first <- getPage page
    when (queryPage query == Nothing && totalRecords first > 10000) $
        throwIO $
            IPGWError "日志超过 1000 页，请用 --page 分页读取"
    seen <- unique S.empty $ pageEntries first
    allEntries <- case queryPage query of
        Just _ -> pure $ pageEntries first
        Nothing -> collect (totalRecords first) 2 seen [pageEntries first]
    let within e =
            let day = localDay $ entryTime e
             in maybe True (<= day) (queryFrom query)
                    && maybe True (>= day) (queryTo query)
                    && (kind /= TrafficLog || matchesTrafficFilter (queryTraffic query) e)
    pure $ LogReport kind query (totalRecords first) (length allEntries) (filter within allEntries)
  where
    unique seen entries = do
        let ids = map entryID entries
            combined = S.union seen $ S.fromList ids
        unless (S.size combined == S.size seen + length ids) $
            throwIO $
                IPGWError "日志分页出现重复记录，未输出可能重复计费的统计；请重试"
        pure combined

reportJSON :: LogReport -> Value
reportJSON report =
    object $
        reportFields report
            ++ [ "entries" .= map entryJSON (reportEntries report)
               , "statistics" .= statistics (reportKind report) (reportEntries report)
               ]
            ++ ["filters" .= trafficFilterJSON filters | filters /= noTrafficFilter]
  where
    filters = queryTraffic $ reportQuery report

reportFields :: LogReport -> [Pair]
reportFields report =
    [ "kind" .= kindName (reportKind report)
    , "source_total" .= sourceTotal report
    , "fetched_records" .= fetchedRecords report
    , "scope" .= (if queryPage query == Nothing then "all_available" else "page" :: Text)
    , "page" .= queryPage query
    , "from" .= fmap show (queryFrom query)
    , "to" .= fmap show (queryTo query)
    , "date_basis" .= (case reportKind report of TrafficLog -> "started_at"; BillingLog -> "settled_at"; PaymentLog -> "paid_at" :: Text)
    , "timezone" .= ("Asia/Shanghai" :: Text)
    ]
  where
    query = reportQuery report

renderReport :: LogReport -> Text
renderReport report =
    T.unlines $
        [(case reportKind report of TrafficLog -> "流量日志"; BillingLog -> "扣费日志（结算清单）"; PaymentLog -> "充值记录")]
            ++ reportContext report
            ++ [renderLogs (reportKind report) (reportEntries report)]

reportContext :: LogReport -> [Text]
reportContext report =
    [ "来源共 " <> number (sourceTotal report) <> " 条；已读取 " <> number (fetchedRecords report) <> " 条；筛选后 " <> number (length $ reportEntries report) <> " 条"
    , "范围：" <> maybe "网站当前可获取的全部分页" (\n -> "第 " <> number n <> " 页（每页 10 条）") (queryPage query)
    , "日期："
        <> maybe "不限" (T.pack . show) (queryFrom query)
        <> " 至 "
        <> maybe "不限" (T.pack . show) (queryTo query)
        <> (case reportKind report of TrafficLog -> "，按上线日期筛选"; BillingLog -> "，按结算日期筛选"; PaymentLog -> "，按缴费日期筛选")
        <> "，含起止日（北京时间）"
    ]
        ++ renderTrafficFilter (queryTraffic query)
  where
    query = reportQuery report
    number :: (Show a) => a -> Text
    number = T.pack . show

ipUsageJSON :: LogReport -> Value
ipUsageJSON report =
    object $
        reportFields report
            ++ [ "source" .= ("/log/detail" :: Text)
               , "group_by" .= ("ip" :: Text)
               , "filters" .= trafficFilterJSON (queryTraffic $ reportQuery report)
               , "ip_count" .= length groups
               , "entries" .= map trafficGroupJSON groups
               , "statistics" .= statistics TrafficLog (reportEntries report)
               ]
  where
    groups = trafficGroups $ reportEntries report

renderIPUsage :: LogReport -> Text
renderIPUsage report =
    T.unlines $
        ["按 IP 用量（上网明细）"]
            ++ reportContext report
            ++ [renderTrafficGroups $ trafficGroups $ reportEntries report]
