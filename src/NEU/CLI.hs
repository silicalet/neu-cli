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
import NEU (versionString)
import NEU.IPGW.Client
import NEU.IPGW.Config
import NEU.IPGW.Protocol
import Options.Applicative hiding (action, value)
import Options.Applicative qualified as OA
import System.Exit (exitFailure)
import System.IO (hFlush, hGetEcho, hIsTerminalDevice, hSetEcho, stderr, stdin)
import Text.Read (readMaybe)

data Command = IPGW Options
data Options = Options (Maybe FilePath) Bool Int Bool Action
data Credentials = Credentials (Maybe Text) (Maybe Text) Bool
data Action = Status | Login Credentials | Logout Bool | Add Text (Maybe Text) Bool | Default Text

parserInfo :: ParserInfo Command
parserInfo = info (helper <*> version <*> commandParser) (fullDesc <> progDesc "东北大学校园服务 CLI")
  where
    version = infoOption versionString (long "version" <> short 'V' <> help "显示版本")

commandParser :: Parser Command
commandParser = hsubparser $ command "ipgw" $ info (helper <*> (IPGW <$> options)) (progDesc "校园网网关管理")

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

runCLI :: IO ()
runCLI = do
    IPGW opts@(Options _ _ _ jsonOutput _) <- execParser parserInfo
    (runIPGW opts `catch` handleGateway jsonOutput) `catch` handleIO jsonOutput
  where
    handleGateway jsonOutput (IPGWError message) = failWith jsonOutput message
    handleIO :: Bool -> IOException -> IO ()
    handleIO jsonOutput _ = failWith jsonOutput "无法读写配置或终端输入，请检查路径和权限"

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
                    config <- loadConfig path
                    account <- case (name, supplied, fromStdin) of
                        (Nothing, Nothing, False) -> either (throwIO . IPGWError) pure $ selectAccount Nothing config
                        _ -> do
                            user <- maybe (maybe (throwIO $ IPGWError "请使用 -u 指定账号") pure $ defaultUsername config) pure name
                            validateName user
                            secret <- case (supplied, fromStdin, selectAccount (Just user) config) of
                                (Nothing, False, Right saved) -> pure $ accountPassword saved
                                _ -> readPassword supplied fromStdin
                            pure $ Account user secret
                    login connection (accountUsername account) (accountPassword account)
                    -- A successful login must remain successful even if status data lags behind.
                    success "登录成功"

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
  where
    humanBytes bytes = scale 1 ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB"]
      where
        scale divisor (unit : next : rest)
            | bytes >= divisor * 1024 = scale (divisor * 1024) (next : rest)
            | otherwise = render divisor unit
        scale divisor [unit] = render divisor unit
        scale _ [] = T.pack (show bytes) <> " B"
        render 1 unit = T.pack (show bytes) <> " " <> unit
        render divisor unit =
            let hundredths = (bytes * 100 + divisor `div` 2) `div` divisor
                (whole, fraction) = hundredths `divMod` 100
             in T.pack (show whole) <> "." <> T.justifyRight 2 '0' (T.pack $ show fraction) <> " " <> unit
