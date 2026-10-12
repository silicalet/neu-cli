{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CASIntegration (casTests)
import CLIIntegration (cliTests)
import Control.Exception (bracket, try)
import Control.Monad (unless)
import Data.Aeson (object, (.=))
import Data.Bits ((.&.))
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Either (isLeft, isRight)
import Data.IORef
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import LibraryTests (libraryTests)
import LogTests (logTests)
import NEU.Auth.CAS.Protocol
import NEU.IPGW.Client
import NEU.IPGW.Config
import NEU.IPGW.Protocol
import NEU.WebVPN (encodeWebVPN, webvpnURL)
import OverviewTests (overviewTests)
import SelfServiceTests (selfServiceTests)
import System.Directory
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import System.Posix.Files (fileMode, getFileStatus)
import System.Process (callProcess)
import TrafficTests (trafficTests)
import UsageTests (usageTests)

assert :: String -> Bool -> IO ()
assert label condition = unless condition $ fail label
right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure
expectFailure :: String -> IO a -> IO ()
expectFailure label action = do
    result <- try (action >> pure ()) :: IO (Either IPGWError ())
    assert label $ isLeft result

main :: IO ()
main = do
    assert "plain JSON" $ isRight $ decodeReply "{\"error\":\"ok\"}"
    offline <- right $ decodeReply "jQuery1124_123({\"error\":\"not_online_error\"});"
    assert "offline status" $ parseStatus offline == Right (Offline offline)
    mapM_
        (\s -> assert "reject malformed JSONP" $ isLeft $ decodeReply s)
        ["<html>{\"error\":\"ok\"}</html>", "cb({});evil()", "cb({}", "{}garbage"]
    assert "unknown status" $ isLeft $ parseStatus $ object []
    formHTML <- T.readFile "test/fixtures/login.html"
    form <- right $ parseLoginForm formHTML
    assert "form values and HTML entity" $ formLT form == "LT-fixture" && formExecution form == "e1s1" && formAction form == "/tpass/login?service=fixture&renew=true"
    assert "missing form" $ isLeft $ parseLoginForm "<html>error</html>"
    assert "missing key" $ isLeft $ extractPublicKey "var other = 1;"
    assert "invalid RSA" $ isLeft $ decodePublicKey "AAAA"
    assert "URL query decoding" $ queryValue "ticket" "http://ipgw.neu.edu.cn/?ticket=ST%2B1" == Right "ST+1"
    assert "missing ticket" $ isLeft $ queryValue "ticket" "http://ipgw.neu.edu.cn/"
    assert "arrears" $ isLeft $ loginResult $ object ["code" .= (1 :: Int), "message" .= ("E2616: Arrearage users." :: T.Text)]
    public <- T.strip <$> T.readFile "test/fixtures/public-key.b64"
    let script = "var publicKeyStr = \"" <> public <> "\";"
    assert "extract public key" $ extractPublicKey script == Right public
    encrypted <- encryptCredentials "20200001" "p密码+&" public >>= right
    encrypted2 <- encryptCredentials "20200001" "p密码+&" public >>= right
    assert "randomized padding" $ encrypted /= encrypted2
    tooLong <- encryptCredentials "u" (T.replicate 300 "x") public
    assert "reject oversized plaintext" $ isLeft tooLong
    withTemp $ \dir -> do
        ciphertext <- right $ B64.decode (TE.encodeUtf8 encrypted)
        BS.writeFile (dir </> "cipher") ciphertext
        callProcess "openssl" ["pkeyutl", "-decrypt", "-inkey", "test/fixtures/test-only-private.pem", "-in", dir </> "cipher", "-out", dir </> "plain", "-pkeyopt", "rsa_padding_mode:pkcs1"]
        plain <- BS.readFile $ dir </> "plain"
        assert "RSA interoperates with OpenSSL (UTF-8 + PKCS1 v1.5)" $ plain == TE.encodeUtf8 "20200001p密码+&"
        webvpnTests
        configTests dir
    protocolTests formHTML script
    casTests formHTML script
    logTests
    overviewTests
    usageTests
    selfServiceTests
    trafficTests
    libraryTests
    cliTests
    putStrLn "All protocol, RSA, configuration and mocked workflow tests passed."

withTemp :: (FilePath -> IO a) -> IO a
withTemp = bracket create removePathForcibly
  where
    create = do
        root <- getTemporaryDirectory
        (path, handle) <- openTempFile root "neu-ipgw-test"
        hClose handle
        removeFile path
        createDirectory path
        pure path

webvpnTests :: IO ()
webvpnTests = do
    let converted input = webvpnURL <$> encodeWebVPN input
        expect input output = assert (T.unpack input) $ converted input == Right output
    expect "https://pass.neu.edu.cn/tpass/login" "https://webvpn.neu.edu.cn/https/62304135386136393339346365373340a0e0b72cc4cb43c8bc1d6f66c806db/tpass/login"
    expect "http://219.216.96.4/eams/homeExt.action" "https://webvpn.neu.edu.cn/http/62304135386136393339346365373340e2b0fd71d8941093ab4e2527/eams/homeExt.action"
    expect "https://ipgw.neu.edu.cn:8800/home" "https://webvpn.neu.edu.cn/https-8800/62304135386136393339346365373340b9f1a328c4cb43c8bc1d6f66c806db/home"
    expect "ipgw.neu.edu.cn" "https://webvpn.neu.edu.cn/http/62304135386136393339346365373340b9f1a328c4cb43c8bc1d6f66c806db"
    expect "https://pass.neu.edu.cn:443/tpass/login?service=a%20b" "https://webvpn.neu.edu.cn/https/62304135386136393339346365373340a0e0b72cc4cb43c8bc1d6f66c806db/tpass/login?service=a%20b"
    assert "reject IPv6 literal" $ isLeft $ encodeWebVPN "http://[2001:db8::1]/path"
    assert "reject unsupported scheme" $ isLeft $ encodeWebVPN "ftp://pass.neu.edu.cn/"
    assert "reject empty URL" $ isLeft $ encodeWebVPN "  "

configTests :: FilePath -> IO ()
configTests dir = do
    let path = dir </> "config/ipgw.json"
        account = Account "u" "secret"
    missing <- loadConfig path
    assert "missing config is empty" $ missing == emptyConfig
    exists <- doesFileExist path
    assert "read does not create config" $ not exists
    assert "default requires account" $ isLeft $ setDefault "missing" emptyConfig
    config <- right $ setDefault "u" $ putAccount account emptyConfig
    saveConfig path config
    restored <- loadConfig path
    assert "config roundtrip" $ restored == config
    mode <- fileMode <$> getFileStatus path
    assert "credentials are mode 0600" $ mode .&. 0o777 == 0o600
    assert "default resolves" $ selectAccount Nothing restored == Right account
    let replaced = putAccount (Account "u" "updated") restored
    assert "add updates without duplicate" $ length (accounts replaced) == 1
    saveConfig path replaced
    BS.writeFile path "not json: secret"
    expectFailure "invalid config fails" $ loadConfig path
    raw <- BS.readFile path
    assert "invalid config preserved" $ raw == "not json: secret"

-- Injected transport means no test ever contacts the real gateway.
mock :: [Reply] -> IO (Client, IO [RequestSpec])
mock replies = do
    remaining <- newIORef replies
    requests <- newIORef []
    let transport spec = do
            modifyIORef' requests (++ [spec])
            responses <- readIORef remaining
            case responses of
                [] -> fail "unexpected network request"
                result : rest -> writeIORef remaining rest >> pure result
        finish = do
            left <- readIORef remaining
            assert "all expected requests performed" $ null left
            readIORef requests
    pure (clientWithTransport transport, finish)

protocolTests :: T.Text -> T.Text -> IO ()
protocolTests html script = do
    let root = Reply "http://ipgw.neu.edu.cn/srun_portal_pc?ac_id=7" ""
        page = Reply "https://pass.neu.edu.cn/tpass/login?service=x" (TE.encodeUtf8 html)
        js = Reply "https://pass.neu.edu.cn/tpass/static/login_neu.js" (TE.encodeUtf8 script)
        ticket = Reply "http://ipgw.neu.edu.cn/srun_portal_sso?ac_id=7&ticket=ST%2Btest" (TE.encodeUtf8 "<title>IP控制网关</title>")
        success = Reply "http://ipgw.neu.edu.cn/v1/srun_portal_sso" "{\"code\":0,\"message\":\"success\"}"
    (client, finish) <- mock [root, page, js, ticket, success]
    login client "user&1" "secret"
    requests <- finish
    let fields = requestForm (requests !! 3)
    assert "encrypted POST, no plaintext password" $ case fields of
        Just values -> lookup "lt" values == Just "LT-fixture" && lookup "ul" values == Just "6" && lookup "pl" values == Just "6" && lookup "password" values == Nothing && lookup "rsa" values /= Nothing
        Nothing -> False
    assert "detected ac_id passed to SSO service" $ "ac_id%3D7" `T.isInfixOf` requestURL (requests !! 1)
    assert "ticket URL encoded" $ "ticket=ST%2Btest" `T.isInfixOf` requestURL (last requests)
    assert "portal referer" $ requestReferer (last requests) /= Nothing
    let authFail = Reply "https://pass.neu.edu.cn/tpass/login" (TE.encodeUtf8 "<title>智慧东大--统一身份认证</title>")
    (bad, finishedBad) <- mock [root, page, js, authFail]
    expectFailure "wrong credentials stop before activation" $ login bad "u" "bad"
    _ <- finishedBad
    let email = Reply "https://pass.neu.edu.cn/tpass/login" (TE.encodeUtf8 "<title>智慧东大</title>")
    (emailClient, finishEmail) <- mock [root, page, js, email, ticket, success]
    login emailClient "u" "p"
    _ <- finishEmail
    let hostile = page {replyBody = TE.encodeUtf8 $ T.replace "/tpass/login?service=fixture&amp;renew=true" "https://evil.example/login" html}
    (unsafe, finishUnsafe) <- mock [root, hostile]
    expectFailure "reject external credential destination" $ login unsafe "u" "p"
    _ <- finishUnsafe
    (existing, finishExisting) <- mock [root, ticket, success]
    login existing "u" "p"
    _ <- finishExisting
    mapM_
        ( \invalidTicket -> do
            (invalid, finishInvalid) <- mock [root, invalidTicket]
            expectFailure "invalid callback never activates gateway" $ login invalid "u" "p"
            _ <- finishInvalid
            pure ()
        )
        [ ticket {replyURL = "http://ipgw.neu.edu.cn/error?ac_id=7&ticket=ST-test"}
        , ticket {replyURL = "http://ipgw.neu.edu.cn/srun_portal_sso?ac_id=8&ticket=ST-test"}
        , ticket {replyURL = "http://ipgw.neu.edu.cn/srun_portal_sso?ac_id=7"}
        ]
    let info = object ["error" .= ("ok" :: T.Text), "user_name" .= ("u&1" :: T.Text), "online_ip" .= ("10.0.0.1" :: T.Text)]
    (out, finishOut) <- mock [root, Reply "http://ipgw.neu.edu.cn/" "neu_logout({\"ecode\":0});"]
    logout out False (Online info)
    outRequests <- finishOut
    assert "logout encodes username" $ "username=u%261" `T.isInfixOf` requestURL (last outRequests)
    (batch, finishBatch) <- mock [root, Reply "http://ipgw.neu.edu.cn/" "{\"code\":0}"]
    logout batch True (Online info)
    batchRequests <- finishBatch
    assert "batch logout uses POST" $ requestForm (last batchRequests) == Just []
    (off, finishOff) <- mock []
    expectFailure "offline logout performs no network calls" $ logout off False (Offline $ object [])
    _ <- finishOff
    (statusClient, finishStatus) <- mock [Reply "http://ipgw.neu.edu.cn/" "cb({\"error\":\"not_online_error\"})"]
    _ <- getStatus statusClient
    statusRequests <- finishStatus
    assert "status requires no SSO" $ case statusRequests of
        [request] -> "/cgi-bin/rad_user_info?" `T.isInfixOf` requestURL request
        _ -> False
