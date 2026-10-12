{-# LANGUAGE OverloadedStrings #-}

-- Real CLI processes, isolated configuration and a loopback-only mock proxy.
module CLIIntegration (cliTests) where

import Control.Monad (forM_, unless)
import Data.Aeson (Value (..), eitherDecodeStrict', encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import Network.HTTP.Types
import Network.URI (parseURI, uriPath)
import Network.Wai
import Network.Wai.Handler.Warp (testWithApplication)
import System.Directory (doesDirectoryExist, doesFileExist, findExecutable)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import System.Process (CreateProcess (env), proc, readCreateProcessWithExitCode)
import System.Timeout (timeout)

assert :: String -> Bool -> IO ()
assert label condition = unless condition $ fail label

type Runner = [String] -> ExitCode -> String -> IO (Text, Text)

-- Pass environment overrides to children only; never change the test runner's
-- environment. readCreateProcessWithExitCode cleans up children on timeout.
runner :: FilePath -> [(String, String)] -> Runner
runner executable environment args expected input = do
    result <-
        timeout 15000000 $
            readCreateProcessWithExitCode
                (proc executable args) {env = Just environment}
                input
    case result of
        Nothing -> fail $ "CLI timed out: " ++ show args
        Just (code, out, err) -> do
            assert ("CLI failed: " ++ show (args, code, out, err)) (code == expected)
            pure (T.pack out, T.pack err)

json :: Text -> IO Value
json text = either fail pure $ eitherDecodeStrict' (TE.encodeUtf8 text)

field :: Text -> Value -> Value
field name (Object fields) = fromMaybe Null $ KM.lookup (Key.fromText name) fields
field _ _ = Null

cliTests :: IO ()
cliTests = withSystemTempDirectory "neu-cli-" $ \directory -> do
    executable <- findExecutable "neu" >>= maybe (fail "Cabal build-tool-depends did not provide neu") pure
    inherited <- getEnvironment
    let environment = overrides [("XDG_CONFIG_HOME", directory)] inherited
        run = runner executable environment
        config = directory </> "neu/ipgw.json"
        checkDefault = do
            stored <- T.readFile config >>= json
            assert "default account preserved" $ field "default_username" stored == String "u"
    forM_ ([["--help"], ["ipgw", "--help"], ["webvpn", "--help"], ["lib", "--help"], ["--version"]] ++ [["ipgw", name, "--help"] | name <- ["login", "traffic-log", "billing-log", "info", "devices", "usage", "payment-log"]] ++ [["lib", name, "--help"] | name <- ["search", "news"]]) $ \args -> do
        _ <- run args ExitSuccess ""
        pure ()
    exists <- doesDirectoryExist $ directory </> "neu"
    assert "help must have no side effects" $ not exists
    forM_
        [ ["ipgw", "bogus"]
        , ["ipgw", "--timeout", "0", "status"]
        , ["ipgw", "add", "-u", "u", "--password-stdin", "-p", "secret"]
        , ["ipgw", "traffic-log", "--from", "2026-02-30"]
        , ["ipgw", "traffic-log", "--page", "0"]
        , ["ipgw", "billing-log", "--from", "2026-10-02", "--to", "2026-10-01"]
        , ["ipgw", "traffic-log", "-p", "secret", "--password-stdin"]
        ]
        $ \args -> do
            _ <- run args (ExitFailure 1) ""
            pure ()
    configExists <- doesFileExist config
    assert "invalid arguments must not create config" $ not configExists
    (webvpnOut, webvpnErr) <- run ["webvpn", "https://pass.neu.edu.cn/tpass/login"] ExitSuccess ""
    assert "webvpn converts without opening a browser" $
        T.strip webvpnOut == "https://webvpn.neu.edu.cn/https/62304135386136393339346365373340a0e0b72cc4cb43c8bc1d6f66c806db/tpass/login" && T.null webvpnErr
    (webvpnJSON, _) <- run ["webvpn", "--json", "ipgw.neu.edu.cn"] ExitSuccess ""
    converted <- json webvpnJSON
    assert "webvpn JSON keeps source and proxy URL" $
        field "source" converted == String "http://ipgw.neu.edu.cn" && field "url" converted == String "https://webvpn.neu.edu.cn/http/62304135386136393339346365373340b9f1a328c4cb43c8bc1d6f66c806db"
    (_, webvpnFailure) <- run ["webvpn", "--json", "ftp://pass.neu.edu.cn"] (ExitFailure 1) ""
    webvpnError <- json webvpnFailure
    assert "webvpn rejects unsupported URLs before opening anything" $ field "ok" webvpnError == Bool False && field "error" webvpnError == String "只支持 http 和 https"
    (out, err) <- run ["ipgw", "add", "-u", "u", "--password-stdin"] ExitSuccess "test-secret\n"
    assert "password must not be printed" $ not $ "test-secret" `T.isInfixOf` (out <> err)
    mode <- fileMode <$> getFileStatus config
    assert "config mode 0600" $ mode .&. 0o777 == 0o600
    _ <- run ["ipgw", "default", "-u", "u"] ExitSuccess ""
    checkDefault
    _ <- run ["ipgw", "--json", "default", "-u", "missing"] (ExitFailure 1) ""
    checkDefault
    T.writeFile config "broken: test-secret"
    (_, failure) <- run ["ipgw", "--json", "default", "-u", "u"] (ExitFailure 1) ""
    failureJSON <- json failure
    assert "JSON config error" $ field "ok" failureJSON == Bool False
    assert "config errors do not leak credentials" $ not $ "test-secret" `T.isInfixOf` failure
    stored <- T.readFile config
    assert "corrupt config preserved" $ stored == "broken: test-secret"
    selfServiceArguments executable environment config failureJSON

    state <- newIORef $ MockState False False KM.empty
    seen <- newIORef (0 :: Int)
    testWithApplication (pure $ mockProxy state seen) $ \port -> do
        let proxy = "http://127.0.0.1:" ++ show port
            proxyEnv =
                [(key, proxy) | key <- ["http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "all_proxy", "ALL_PROXY"]]
                    ++ [("no_proxy", ""), ("NO_PROXY", "")]
            networkRun = runner executable (overrides proxyEnv environment)
        networkTests networkRun state seen
    putStrLn "CLI and local HTTP integration checks passed (cookies, redirects, proxy, logout, JSON, config)."

overrides :: [(String, String)] -> [(String, String)] -> [(String, String)]
overrides replacement original = replacement ++ filter ((`notElem` map fst replacement) . fst) original

-- A loopback-only trap also counts CONNECT requests: malformed arguments must
-- fail before reading credentials or starting the HTTPS authentication flow.
selfServiceArguments :: FilePath -> [(String, String)] -> FilePath -> Value -> IO ()
selfServiceArguments executable environment config configError = do
    seen <- newIORef (0 :: Int)
    original <- BS.readFile config
    let trap _ respond = do
            modifyIORef' seen (+ 1)
            respond $ responseLBS status502 [] "unexpected network request"
    testWithApplication (pure trap) $ \port -> do
        let proxy = "http://127.0.0.1:" ++ show port
            proxyEnv = [(key, proxy) | key <- ["http_proxy", "https_proxy", "HTTP_PROXY", "HTTPS_PROXY", "all_proxy", "ALL_PROXY"]] ++ [("no_proxy", ""), ("NO_PROXY", "")]
            run = runner executable (overrides proxyEnv environment)
        forM_ [["info"], ["devices"], ["usage"], ["usage", "--by-ip"], ["traffic-log"], ["payment-log"]] $ \args -> do
            _ <- run (["ipgw"] ++ args ++ ["--help"]) ExitSuccess ""
            (out, err) <- run (["ipgw", "--json"] ++ args ++ ["-u", "fixture-user", "-p", "fixture-secret", "--password-stdin"]) (ExitFailure 1) ""
            errorJSON <- json err
            assert "credential conflicts precede config read" $ field "ok" errorJSON == Bool False && errorJSON /= configError && T.null out
            assert "new commands never leak explicit credentials" $ not $ "fixture-secret" `T.isInfixOf` err
        forM_
            [ ["usage", "--from", "2026-02-30"]
            , ["usage", "--daily", "--from", "2026-1-01"]
            , ["usage", "--monthly", "--from", "2026-13"]
            , ["usage", "--monthly", "--from", "2026-10-01"]
            , ["usage", "--daily", "--from", "2026-10"]
            , ["usage", "--from", "2026-10-02", "--to", "2026-10-01"]
            , ["usage", "--monthly", "--from", "2026-10", "--to", "2026-09"]
            , ["payment-log", "--from", "2026-10-02", "--to", "2026-10-01"]
            ]
            $ \args -> do
                (out, err) <- run (["ipgw", "--json"] ++ args ++ ["-u", "fixture-user", "--password-stdin"]) (ExitFailure 1) ""
                errorJSON <- json err
                assert "invalid ranges precede config/password/network" $ T.null out && field "ok" errorJSON == Bool False && errorJSON /= configError
        forM_
            ( [ command ++ bounds
              | command <- [["traffic-log"], ["usage", "--by-ip"]]
              , bounds <- [["--min-bytes", "2", "--max-bytes", "1"], ["--min-seconds", "1", "--max-seconds", "0"]]
              ]
                ++ [ command ++ [flag, blank]
                   | command <- [["traffic-log"], ["usage", "--by-ip"]]
                   , flag <- ["--ip", "--product"]
                   , blank <- ["", "   "]
                   ]
                ++ [ ["usage"] ++ mode ++ flags
                   | mode <- [[], ["--daily"], ["--monthly"]]
                   , flags <- [["--page", "1"], ["--ip", "10.0.0.1"], ["--product", "校园网"], ["--min-bytes", "0"], ["--max-bytes", "0"], ["--min-seconds", "0"], ["--max-seconds", "0"]]
                   ]
                ++ [ ["usage", "--by-ip", "--from", day]
                   | day <- ["2026-02-30", "2026-10", "2026-1-01"]
                   ]
                ++ [["usage", "--by-ip", "--from", "2026-10-02", "--to", "2026-10-01"]]
            )
            $ \args -> do
                (out, err) <- run (["ipgw", "--json"] ++ args ++ ["-u", "fixture-user", "--password-stdin"]) (ExitFailure 1) ""
                errorJSON <- json err
                assert "traffic validation precedes config/password/network" $
                    T.null out && field "ok" errorJSON == Bool False && errorJSON /= configError
        forM_
            [ command ++ [flag, value]
            | command <- [["traffic-log"], ["usage", "--by-ip"]]
            , flag <- ["--min-bytes", "--max-bytes", "--min-seconds", "--max-seconds"]
            , value <- ["-1", "1.5", "1e3", "1KiB", "+1", " 1", ""]
            ]
            $ \args -> do
                (out, err) <- run (["ipgw", "--json"] ++ args ++ ["-u", "fixture-user", "--password-stdin"]) (ExitFailure 1) ""
                assert "invalid traffic counts fail in the parser before credentials" $
                    T.null out && eitherDecodeStrict' (TE.encodeUtf8 err) /= Right configError
        forM_
            ( [ ["usage", first, second]
              | first <- ["--daily", "--monthly", "--by-ip"]
              , second <- ["--daily", "--monthly", "--by-ip"]
              , first /= second
              ]
                ++ [ ["payment-log", "--from", "2026-02-30"]
                   , ["payment-log", "--page", "0"]
                   , ["usage", "--by-ip", "--page", "0"]
                   , ["usage", "--by-ip", "--page", "1000001"]
                   , ["billing-log", "--ip", "10.0.0.1"]
                   , ["payment-log", "--product", "校园网"]
                   ]
            )
            $ \args -> do
                (out, err) <- run (["ipgw", "--json"] ++ args ++ ["-u", "fixture-user", "--password-stdin"]) (ExitFailure 1) ""
                assert "syntax errors precede config/password/network" $
                    T.null out && eitherDecodeStrict' (TE.encodeUtf8 err) /= Right configError
        forM_
            ( [ command ++ bounds
              | command <- [["traffic-log"], ["usage", "--by-ip"]]
              , bounds <-
                    [ ["--min-bytes", "0", "--max-bytes", "0", "--min-seconds", "0", "--max-seconds", "0"]
                    , ["--min-bytes", "18446744073709551616", "--max-bytes", "18446744073709551616"]
                    , ["--ip", "10.0.0.1", "--ip", "2001:db8::1", "--product", "校园网", "--product", "移动", "--page", "1", "--from", "2026-10-01", "--to", "2026-10-01"]
                    ]
              ]
                ++ [["usage"], ["usage", "--daily"], ["usage", "--monthly", "--from", "2026-10", "--to", "2026-10"], ["usage", "--by-ip"]]
            )
            $ \args -> do
                (out, err) <- run (["ipgw", "--json"] ++ args ++ ["-u", "fixture-user", "--password-stdin"]) (ExitFailure 1) ""
                errorJSON <- json err
                assert "valid modes and inclusive zero or large bounds reach normal config validation" $
                    T.null out && errorJSON == configError
    count <- readIORef seen
    stored <- BS.readFile config
    assert "new-command help and invalid arguments perform no network or config writes" $ count == 0 && stored == original

data MockState = MockState
    {online :: Bool, bad :: Bool, extraFields :: KM.KeyMap Value}

mockProxy :: IORef MockState -> IORef Int -> Application
mockProxy state seen request respond = do
    assert "proxy target must be gateway" $ requestHeaderHost request == Just "ipgw.neu.edu.cn"
    atomicModifyIORef' seen $ \n -> (n + 1, ())
    current <- readIORef state
    let raw = rawPathInfo request
        path = maybe raw (BC.pack . uriPath) $ parseURI (BC.unpack raw)
        parameter key = lookup key (queryString request) >>= id
        checkCookie =
            assert "session cookie survives requests and redirects" $
                maybe False (BS.isInfixOf "session=fixture") $
                    lookup hCookie (requestHeaders request)
        reply status headers body = respond $ responseLBS status headers body
    if bad current
        then reply status503 [] ""
        else case path of
            "/cgi-bin/rad_user_info" -> do
                callback <- maybe (fail "missing JSONP callback") pure $ parameter "callback"
                let fields =
                        if online current
                            then extraFields current <> KM.fromList [("error", String "ok"), ("user_name", String "fixture-user"), ("online_ip", String "10.0.0.1")]
                            else KM.singleton "error" (String "not_online_error")
                reply
                    status200
                    [("Set-Cookie", "session=fixture; Path=/")]
                    (BL.fromStrict callback <> "(" <> encode (Object fields) <> ");")
            root | root == "" || root == "/" -> do
                checkCookie
                reply status302 [(hLocation, "http://ipgw.neu.edu.cn/srun_portal_pc?ac_id=7")] ""
            "/srun_portal_pc" -> do
                checkCookie
                reply status200 [] "<title>Gateway</title>"
            "/cgi-bin/srun_portal" -> do
                assert "logout username" $ parameter "username" == Just "fixture-user"
                assert "logout ac_id" $ parameter "ac_id" == Just "7"
                reply status200 [] "neu_logout({\"ecode\":0})"
            "/v1/batch-online-drop" -> do
                assert "batch logout uses POST" $ requestMethod request == methodPost
                checkCookie
                reply status200 [] "{\"code\":0}"
            _ -> fail $ "unexpected proxy request: " ++ show path

networkTests :: Runner -> IORef MockState -> IORef Int -> IO ()
networkTests run state seen = do
    let ok args = fst <$> run args ExitSuccess ""
        setFields pairs = modifyIORef' state $ \s -> s {extraFields = KM.fromList pairs}
    offline <- ok ["ipgw", "--timeout", "2", "--json", "status"] >>= json
    assert "offline JSON" $ field "online" offline == Bool False
    count <- readIORef seen
    assert "status only contacts mock proxy, no SSO" $ count == 1
    _ <- run ["ipgw", "o"] (ExitFailure 1) ""
    modifyIORef' state $ \s -> s {online = True}
    setFields [("online_ip6", String "2001:db8::1234"), ("sum_bytes", Number 1610612736)]
    result <- ok ["ipgw", "s", "--json"] >>= json
    assert "online JSON" $ field "online" result == Bool True
    assert "raw IPv6 JSON" $ field "online_ip6" (field "data" result) == String "2001:db8::1234"
    assert "raw byte count JSON" $ field "sum_bytes" (field "data" result) == Number 1610612736
    readable <- ok ["ipgw", "status"]
    forM_ ["IPv4：10.0.0.1", "IPv6：2001:db8::1234", "已用流量：1.50 GiB（1610612736 字节）"] $ \text ->
        assert "readable status" $ text `T.isInfixOf` readable
    forM_
        [ (Number 0, "0", "0 B")
        , (Number 1023, "1023", "1023 B")
        , (Number 1024, "1024", "1.00 KiB")
        , (Number 1048576, "1048576", "1.00 MiB")
        , (Number 1099511627776, "1099511627776", "1.00 TiB")
        , (String "1536", "1536", "1.50 KiB")
        ]
        $ \(bytes, raw, human) -> do
            setFields [("online_ip6", String ""), ("sum_bytes", bytes)]
            output <- ok ["ipgw", "s"]
            assert "empty IPv6" $ "IPv6：未提供" `T.isInfixOf` output
            assert "human readable units" $ ("已用流量：" <> human <> "（" <> raw <> " 字节）") `T.isInfixOf` output
    forM_ [[], [("sum_bytes", Null)], [("sum_bytes", Number (-1))], [("sum_bytes", String "bad")]] $ \fields -> do
        setFields fields
        output <- ok ["ipgw", "s"]
        assert "missing IPv6" $ "IPv6：未提供" `T.isInfixOf` output
        assert "unknown traffic" $ "已用流量：未知" `T.isInfixOf` output
    -- Config is still corrupt: already-online login must not read it or prompt.
    forM_ [["ipgw", "i"], ["ipgw", "logout"], ["ipgw", "o", "--all"]] $ \args -> do
        _ <- ok args
        pure ()
    modifyIORef' state $ \s -> s {bad = True}
    (_, failure) <- run ["ipgw", "--json", "status"] (ExitFailure 1) ""
    errorJSON <- json failure
    assert "HTTP 503 error" $ case field "error" errorJSON of
        String message -> "503" `T.isInfixOf` message
        _ -> False
