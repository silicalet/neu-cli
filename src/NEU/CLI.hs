{-# LANGUAGE OverloadedStrings #-}

module NEU.CLI (runCLI, parserInfo) where

import Control.Exception (IOException, bracket, catch, throwIO)
import Control.Monad (when)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import NEU (versionString)
import NEU.IPGW.Client
import NEU.IPGW.Config
import NEU.IPGW.Logs (LogKind (..), humanBytes)
import NEU.IPGW.Overview qualified as Overview
import NEU.IPGW.Protocol
import NEU.IPGW.SelfService qualified as SS
import NEU.IPGW.Traffic (TrafficFilter (..), noTrafficFilter)
import NEU.IPGW.Usage qualified as Usage
import NEU.Library qualified as Library
import NEU.Library.Access qualified as LibraryAccess
import NEU.WebVPN (WebVPNURL (..), encodeWebVPN)
import Options.Applicative hiding (action, value)
import Options.Applicative qualified as OA
import System.Exit (exitFailure)
import System.IO (hFlush, hGetEcho, hIsTerminalDevice, hSetEcho, stderr, stdin)
import System.Process (createProcess, proc)
import Text.Read (readMaybe)

data Command = IPGW Options | WebVPN Bool (Maybe FilePath) Bool Text | LibraryCommand LibraryOptions
data Options = Options (Maybe FilePath) Bool Int Bool Action
data Credentials = Credentials (Maybe Text) (Maybe Text) Bool
data Action
    = Status
    | Login Credentials
    | Logout Bool
    | Add Text (Maybe Text) Bool
    | Default Text
    | Logs LogKind Credentials SS.LogQuery
    | Info Credentials
    | Devices Credentials
    | Usage Credentials UsageMode (Maybe Text) (Maybe Text) (Maybe Int) TrafficFilter

data UsageMode = ChartUsage Usage.UsagePeriod | IPUsage

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> version <*> commandParser) (fullDesc <> progDesc "东北大学校园服务 CLI")
  where
    version = infoOption versionString (long "version" <> short 'V' <> help "显示版本")

commandParser :: Parser Command
commandParser =
    hsubparser $
        command "ipgw" (info (helper <*> (IPGW <$> options)) (progDesc "校园网网关管理"))
            <> command "webvpn" (info (helper <*> webvpnParser) (progDesc "把校内地址转换为 WebVPN 地址"))
            <> command "lib" (info (helper <*> (LibraryCommand <$> libraryOptions)) (progDesc "图书馆只读查询"))

webvpnParser :: Parser Command
webvpnParser =
    WebVPN
        <$> switch (long "json" <> help "以 JSON 输出结果和错误")
        <*> optional (strOption (long "browser" <> metavar "COMMAND" <> help "用该命令打开转换后的地址"))
        <*> switch (long "open" <> short 'o' <> help "用默认浏览器打开；指定 --browser 时也会打开")
        <*> (T.pack <$> strArgument (metavar "URL" <> help "校内 http(s) 地址；省略协议时按 http"))

data LibraryOptions = LibraryOptions (Maybe FilePath) Bool Int Bool Credentials LibraryAction
data LibraryAction = Search Text Text Text Int | Notices Text Int

libraryOptions :: Parser LibraryOptions
libraryOptions =
    LibraryOptions
        <$> optional (strOption (long "config" <> metavar "FILE" <> help "配置文件（默认 $XDG_CONFIG_HOME/neu/ipgw.json）"))
        <*> switch (long "bypass-system-proxy" <> help "忽略 HTTP(S) 代理环境变量")
        <*> option timeoutReader (long "timeout" <> OA.value 10 <> showDefault <> metavar "SECONDS" <> help "单次请求超时（1–300 秒）")
        <*> switch (long "json" <> help "以 JSON 输出结果和错误")
        <*> credentials
        <*> hsubparser
            ( command "search" (info (helper <*> searchParser) (progDesc "检索馆藏"))
                <> command "news" (info (helper <*> newsParser) (progDesc "图书馆公告或资源动态"))
            )
  where
    searchParser =
        Search
            <$> (T.pack <$> strArgument (metavar "QUERY" <> help "检索词"))
            <*> (T.pack <$> option fieldReader (long "field" <> OA.value "WRD" <> showDefault <> metavar "CODE" <> help "WRD/WTI/TIT/WAU/WSU/WPU/ISS/ISB/CAL/CLC/SYS/BAR"))
            <*> (T.pack <$> option baseReader (long "base" <> OA.value "NEU01" <> showDefault <> metavar "BASE" <> help "NEU01 中文，NEU09 西文"))
            <*> option limitReader (long "limit" <> OA.value 10 <> showDefault <> metavar "N" <> help "最多返回条数（1–50）")
    newsParser =
        Notices
            <$> (T.pack <$> option newsReader (long "kind" <> OA.value "notice" <> showDefault <> metavar "KIND" <> help "notice 公告，resource 资源动态"))
            <*> option limitReader (long "limit" <> OA.value 10 <> showDefault <> metavar "N" <> help "最多返回条数（1–50）")
    fieldReader = enumReader ["WRD", "WTI", "TIT", "WAU", "WSU", "WPU", "ISS", "ISB", "CAL", "CLC", "SYS", "BAR"] "未知检索字段"
    baseReader = enumReader ["NEU01", "NEU09"] "文献库只支持 NEU01 或 NEU09"
    newsReader = enumReader ["notice", "resource"] "类型只支持 notice 或 resource"
    limitReader = eitherReader $ \input -> case reads input of
        [(n, "")] | n >= (1 :: Int) && n <= 50 -> Right n
        _ -> Left "数量必须为 1–50 的整数"
    timeoutReader = eitherReader $ \input -> case reads input of
        [(n, "")] | n >= (1 :: Int) && n <= 300 -> Right n
        _ -> Left "超时必须为 1–300 的整数"
    enumReader allowed message = eitherReader $ \input -> if input `elem` allowed then Right input else Left message

options :: Parser Options
options =
    Options
        <$> optional (strOption (long "config" <> metavar "FILE" <> help "配置文件（默认 $XDG_CONFIG_HOME/neu/ipgw.json）"))
        <*> switch (long "bypass-system-proxy" <> help "忽略 HTTP(S) 代理环境变量")
        <*> option positive (long "timeout" <> OA.value 10 <> showDefault <> metavar "SECONDS" <> help "单次请求超时（1–300 秒）")
        <*> switch (long "json" <> help "以 JSON 输出结果和错误")
        <*> hsubparser
            ( sub "status" Status "查询当前设备状态"
                <> sub "s" Status "status 的别名"
                <> cmd "login" (Login <$> credentials) "登录校园网"
                <> cmd "i" (Login <$> credentials) "login 的别名"
                <> cmd "logout" logoutParser "注销当前设备或所有设备"
                <> cmd "o" logoutParser "logout 的别名"
                <> cmd "add" (Add <$> username <*> password <*> passwordStdin) "添加或更新账号（明文保存到权限 0600 的文件）"
                <> cmd "default" (Default <$> username) "设置默认账号"
                <> cmd "traffic-log" (Logs TrafficLog <$> credentials <*> logQuery TrafficLog) "上网流量日志与统计（默认读取全部分页）"
                <> cmd "billing-log" (Logs BillingLog <$> credentials <*> logQuery BillingLog) "结算扣费日志与统计（不含充值记录）"
                <> cmd "payment-log" (Logs PaymentLog <$> credentials <*> logQuery PaymentLog) "充值记录与按缴费类型统计（默认读取全部分页）"
                <> cmd "info" (Info <$> credentials) "账号状态与各产品概况（只读）"
                <> cmd "devices" (Devices <$> credentials) "在线设备明细与数量（只读）"
                <> cmd "usage" usageParser "日/月流量或按 IP 汇总流量日志（默认按日；只读）"
            )
  where
    cmd name parser description = command name (info (helper <*> parser) (progDesc description))
    sub name commandValue description = cmd name (pure commandValue) description
    logoutParser = Logout <$> switch (long "all" <> short 'a' <> help "下线该账号所有设备")
    positive = eitherReader $ \input -> case reads input of
        [(n, "")] | n >= (1 :: Int) && n <= 300 -> Right n
        _ -> Left "超时必须为 1–300 的整数"

username :: Parser Text
username = T.pack <$> strOption (long "username" <> short 'u' <> metavar "USERNAME" <> help "学工号")
password :: Parser (Maybe Text)
password = optional $ T.pack <$> strOption (long "password" <> short 'p' <> metavar "PASSWORD" <> help "直接指定密码（可能出现在进程列表中；建议交互输入）")
passwordStdin :: Parser Bool
passwordStdin = switch (long "password-stdin" <> help "从标准输入读取一行密码")
credentials :: Parser Credentials
credentials = Credentials <$> optional username <*> password <*> passwordStdin

logQuery :: LogKind -> Parser SS.LogQuery
logQuery kind =
    SS.LogQuery
        <$> pageOption
        <*> optional (option dayReader (long "from" <> metavar "YYYY-MM-DD" <> help "起始日期（含当天），按上线/结算/缴费日期筛选"))
        <*> optional (option dayReader (long "to" <> metavar "YYYY-MM-DD" <> help "结束日期（含当天），北京时间"))
        <*> (if kind == TrafficLog then trafficFilter else pure noTrafficFilter)

pageOption :: Parser (Maybe Int)
pageOption = optional $ option pageNumber (long "page" <> metavar "N" <> help "只读取指定日志页，每页 10 条；usage 仅限 --by-ip，统计仅覆盖该页")
  where
    pageNumber = eitherReader $ \input -> case reads input of
        [(n, "")] | n >= (1 :: Int) && n <= 1000000 -> Right n
        _ -> Left "页码必须为 1–1000000 的整数"

parseDay :: Text -> Either Text Day
parseDay input = case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack input) of
    Just day | T.length input == 10 && T.pack (formatTime defaultTimeLocale "%Y-%m-%d" day) == input -> Right day
    _ -> Left "日期必须是有效的 YYYY-MM-DD"

dayReader :: ReadM Day
dayReader = eitherReader $ either (Left . T.unpack) Right . parseDay . T.pack

trafficFilter :: Parser TrafficFilter
trafficFilter =
    TrafficFilter
        <$> many (strOption (long "ip" <> metavar "IP" <> help "按日志原始 IP 精确匹配；可重复（同类或、不同条件且）；usage 仅限 --by-ip"))
        <*> many (strOption (long "product" <> metavar "NAME" <> help "按产品名精确匹配；可重复；usage 仅限 --by-ip"))
        <*> bound "min-bytes" "每条日志最少字节数（含边界）；usage 仅限 --by-ip"
        <*> bound "max-bytes" "每条日志最多字节数（含边界）；usage 仅限 --by-ip"
        <*> bound "min-seconds" "每条日志最短秒数（含边界）；usage 仅限 --by-ip"
        <*> bound "max-seconds" "每条日志最长秒数（含边界）；usage 仅限 --by-ip"
  where
    bound name description = optional $ option count (long name <> metavar "N" <> help description)
    count = eitherReader $ \input ->
        if not (null input) && all (\c -> c >= '0' && c <= '9') input
            then maybe (Left "计数必须为非负整数（不含单位）") Right (readMaybe input)
            else Left "计数必须为非负整数（不含单位）"

usageParser :: Parser Action
usageParser =
    Usage
        <$> credentials
        <*> ( flag' (ChartUsage Usage.Daily) (long "daily" <> help "日流量（默认）；与 --monthly、--by-ip 互斥")
                <|> flag' (ChartUsage Usage.Monthly) (long "monthly" <> help "月流量；与 --daily、--by-ip 互斥")
                <|> flag' IPUsage (long "by-ip" <> help "按原始 IP 汇总 /log/detail 日志（不是物理设备）；与 --daily、--monthly 互斥")
                <|> pure (ChartUsage Usage.Daily)
            )
        <*> optional (strOption (long "from" <> metavar "PERIOD" <> help "起始日 YYYY-MM-DD 或月 YYYY-MM（含当期）；--by-ip 按上线日期，跨日会话整条计入上线日"))
        <*> optional (strOption (long "to" <> metavar "PERIOD" <> help "结束日/月（含当期；北京时间）；--by-ip 必须为 YYYY-MM-DD"))
        <*> pageOption
        <*> trafficFilter

runCLI :: IO ()
runCLI = do
    commandValue <- execParser parserInfo
    case commandValue of
        IPGW opts@(Options _ _ _ jsonOutput _) ->
            (runIPGW opts `catch` handleGateway jsonOutput) `catch` handleIO jsonOutput
        WebVPN jsonOutput browser open target ->
            runWebVPN jsonOutput browser open target `catch` handleIO jsonOutput
        LibraryCommand opts@(LibraryOptions _ _ _ jsonOutput _ _) ->
            (runLibrary opts `catch` handleLibrary jsonOutput) `catch` handleIO jsonOutput
  where
    handleGateway jsonOutput (IPGWError message) = failWith jsonOutput message
    handleLibrary jsonOutput (LibraryAccess.LibraryError message) = failWith jsonOutput message
    handleIO :: Bool -> IOException -> IO ()
    handleIO jsonOutput _ = failWith jsonOutput "无法读写配置、启动浏览器或读取终端输入，请检查路径和权限"

runLibrary :: LibraryOptions -> IO ()
runLibrary (LibraryOptions override bypass timeout jsonOutput creds action) = do
    path <- maybe configPath pure override
    account <- resolveAccount path creds
    (route, connection) <- LibraryAccess.openLibrary bypass timeout (accountUsername account) (accountPassword account)
    let routeText = LibraryAccess.routeName route
    case action of
        Search text field localBase limit -> do
            when (T.null $ T.strip text) $ throwIO $ LibraryAccess.LibraryError "检索词不能为空"
            hits <- LibraryAccess.fetchCatalog connection (Library.CatalogQuery text field localBase limit)
            emit jsonOutput (Library.hitsJSON routeText hits) (Library.renderHits hits)
        Notices kind limit -> do
            items <- LibraryAccess.fetchNews connection kind
            let shown = take limit items
            emit jsonOutput (Library.newsJSON routeText kind shown) (Library.renderNews shown)

runWebVPN :: Bool -> Maybe FilePath -> Bool -> Text -> IO ()
runWebVPN jsonOutput browser open target = case encodeWebVPN target of
    Left message -> failWith jsonOutput message
    Right converted -> do
        emit jsonOutput (object ["url" .= webvpnURL converted, "source" .= sourceURL converted]) (webvpnURL converted)
        when (open || browser /= Nothing) $
            launchBrowser (fromMaybe "xdg-open" browser) (T.unpack $ webvpnURL converted)

launchBrowser :: FilePath -> String -> IO ()
launchBrowser browserCommand url = do
    _ <- createProcess (proc browserCommand [url])
    pure ()

failWith :: Bool -> Text -> IO a
failWith jsonOutput message = do
    if jsonOutput
        then BL.hPutStrLn stderr (encode $ object ["ok" .= False, "error" .= message])
        else T.hPutStrLn stderr ("错误：" <> message)
    exitFailure

runIPGW :: Options -> IO ()
runIPGW (Options override bypass timeout jsonOutput action) = do
    path <- maybe configPath pure override
    let success message = emit jsonOutput (object ["ok" .= True, "message" .= message]) message
        client = newClient bypass timeout
        selfService creds = do
            account <- resolveAccount path creds
            connection <- SS.newSelfService bypass timeout
            SS.authenticateSelfService connection (accountUsername account) (accountPassword account)
            pure connection
    case action of
        Add name supplied fromStdin -> do
            validateName name
            secret <- readPassword supplied fromStdin
            config <- loadConfig path
            saveConfig path (putAccount (Account name secret) config)
            success "账号已保存"
        Default name -> do
            config <- loadConfig path
            updated <- either (throwIO . IPGWError) pure (setDefault name config)
            saveConfig path updated
            success "默认账号已设置"
        Status -> client >>= getStatus >>= printStatus jsonOutput
        Logs kind creds query -> do
            either (throwIO . IPGWError) pure $ SS.validateLogQuery kind query
            connection <- selfService creds
            report <- SS.fetchLogs connection kind query
            emit jsonOutput (SS.reportJSON report) (SS.renderReport report)
        Info creds -> do
            overview <- selfService creds >>= SS.fetchOverview
            emit jsonOutput (Overview.infoJSON overview) (Overview.renderInfo overview)
        Devices creds -> do
            overview <- selfService creds >>= SS.fetchOverview
            emit jsonOutput (Overview.devicesJSON overview) (Overview.renderDevices overview)
        Usage creds mode from to page filters -> case mode of
            ChartUsage period -> do
                when (page /= Nothing || filters /= noTrafficFilter) $
                    throwIO $
                        IPGWError "usage 的 --page、--ip、--product 和字节/秒数筛选只能与 --by-ip 一起使用"
                let query = Usage.UsageQuery period from to
                either (throwIO . IPGWError) pure $ Usage.validateUsageQuery query
                connection <- selfService creds
                report <- SS.fetchUsage connection query
                emit jsonOutput (Usage.usageJSON report) (Usage.renderUsage report)
            IPUsage -> do
                first <- either (throwIO . IPGWError) pure $ traverse parseDay from
                lastDay <- either (throwIO . IPGWError) pure $ traverse parseDay to
                let query = SS.LogQuery page first lastDay filters
                either (throwIO . IPGWError) pure $ SS.validateLogQuery TrafficLog query
                connection <- selfService creds
                report <- SS.fetchLogs connection TrafficLog query
                emit jsonOutput (SS.ipUsageJSON report) (SS.renderIPUsage report)
        Logout allDevices -> do
            connection <- client
            status <- getStatus connection
            logout connection allDevices status
            success $ if allDevices then "已下线所有设备" else "已注销当前设备"
        Login (Credentials name supplied fromStdin) -> do
            when (fromStdin && supplied /= Nothing) $ throwIO $ IPGWError "--password 与 --password-stdin 不能同时使用"
            connection <- client
            status <- getStatus connection
            case status of
                Online _ -> printStatus jsonOutput status
                Offline _ -> do
                    account <- resolveAccount path (Credentials name supplied fromStdin)
                    login connection (accountUsername account) (accountPassword account)
                    -- A successful login must remain successful even if status data lags behind.
                    success "登录成功"

resolveAccount :: FilePath -> Credentials -> IO Account
resolveAccount path (Credentials name supplied fromStdin) = do
    when (fromStdin && supplied /= Nothing) $ throwIO $ IPGWError "--password 与 --password-stdin 不能同时使用"
    config <- loadConfig path
    case (name, supplied, fromStdin) of
        (Nothing, Nothing, False) -> either (throwIO . IPGWError) pure $ selectAccount Nothing config
        _ -> do
            user <- maybe (maybe (throwIO $ IPGWError "请使用 -u 指定账号") pure $ defaultUsername config) pure name
            validateName user
            secret <- case (supplied, fromStdin, selectAccount (Just user) config) of
                (Nothing, False, Right saved) -> pure $ accountPassword saved
                _ -> readPassword supplied fromStdin
            pure $ Account user secret

validateName :: Text -> IO ()
validateName name = when (T.null $ T.strip name) $ throwIO $ IPGWError "账号不能为空"

readPassword :: Maybe Text -> Bool -> IO Text
readPassword supplied fromStdin = do
    when (fromStdin && supplied /= Nothing) $ throwIO $ IPGWError "--password 与 --password-stdin 不能同时使用"
    secret <- case supplied of
        Just value -> pure value
        Nothing | fromStdin -> T.getLine
        Nothing -> do
            terminal <- hIsTerminalDevice stdin
            when (not terminal) $ throwIO $ IPGWError "非交互环境请使用 --password-stdin"
            T.hPutStr stderr "密码："
            hFlush stderr
            bracket (hGetEcho stdin <* hSetEcho stdin False) (\echo -> hSetEcho stdin echo >> T.hPutStrLn stderr "") (const T.getLine)
    when (T.null secret) $ throwIO $ IPGWError "密码不能为空"
    pure secret

emit :: Bool -> Value -> Text -> IO ()
emit jsonOutput value message = if jsonOutput then BL.putStrLn (encode value) else T.putStrLn message

printStatus :: Bool -> GatewayStatus -> IO ()
printStatus jsonOutput status = case status of
    Offline value -> emit jsonOutput (object ["online" .= False, "data" .= value]) "当前设备未登录校园网"
    Online value ->
        emit jsonOutput (object ["online" .= True, "data" .= value]) $
            T.unlines
                [ "状态：在线"
                , "账号：" <> field "user_name"
                , "IPv4：" <> address "online_ip"
                , "IPv6：" <> address "online_ip6"
                , "套餐：" <> field "billing_name"
                , "已用流量：" <> traffic (fieldText "sum_bytes" value)
                , "余额：" <> field "user_balance"
                ]
      where
        field key = fromMaybe "未知" $ fieldText key value
        address key = case T.strip <$> fieldText key value of
            Just text | not (T.null text) -> text
            _ -> "未提供"

-- Keep the exact byte count alongside IEC units; use integer arithmetic so
-- large counters never lose precision when rounded to two decimal places.
traffic :: Maybe Text -> Text
traffic raw = case raw >>= readMaybe . T.unpack . T.strip of
    Just bytes | bytes >= (0 :: Integer) -> humanBytes bytes <> "（" <> T.pack (show bytes) <> " 字节）"
    _ -> "未知"
