{-# LANGUAGE OverloadedStrings #-}

module CASIntegration (casTests) where

import Control.Exception (try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.IORef
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import NEU.Auth.CAS
import NEU.HTTP.Session
import NEU.HTTP.URL (queryValue)
import Network.HTTP.Types
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)

assert :: String -> Bool -> IO ()
assert label ok = unless ok $ fail label

right :: (Show e) => Either e a -> IO a
right = either (fail . show) pure

mock :: [Reply] -> IO (Session, IO [RequestSpec])
mock replies = do
    pending <- newIORef replies
    seen <- newIORef []
    let send spec = do
            modifyIORef' seen (++ [spec])
            rest <- readIORef pending
            case rest of
                [] -> fail "unexpected CAS request"
                r : rs -> writeIORef pending rs >> pure r
        finish = do
            remaining <- readIORef pending
            assert "all CAS requests consumed" $ null remaining
            readIORef seen
    pure (sessionWithTransport send, finish)

casFailure :: String -> CASError -> IO Reply -> IO ()
casFailure label expected action = do
    result <- try action
    assert label $ case result of
        Left err -> err == expected
        Right _ -> False

serviceFailure :: String -> IO a -> IO ()
serviceFailure label action = do
    result <- try (action >> pure ()) :: IO (Either ServiceError ())
    assert label $ isLeft result

casTests :: Text -> Text -> IO ()
casTests html script = do
    let selfURL = "http://ipgw.neu.edu.cn:8800/sso/neusoft/index"
        selfOrigins = ["http://ipgw.neu.edu.cn:8800", "https://ipgw.neu.edu.cn:8800"]
        gatewayURL = "http://ipgw.neu.edu.cn/srun_portal_sso?ac_id=7"
        page = Reply "https://pass.neu.edu.cn/tpass/login" $ TE.encodeUtf8 html
        js = Reply "https://pass.neu.edu.cn/tpass/static/login_neu.js" $ TE.encodeUtf8 script
        home = Reply "https://ipgw.neu.edu.cn:8800/home" "<title>Account</title>"
        ticket = Reply (gatewayURL <> "&ticket=ST-test") ""
        email = page {replyBody = TE.encodeUtf8 "<title>智慧东大</title>"}
        failed = page {replyBody = TE.encodeUtf8 "<title>智慧东大--统一身份认证</title>"}
    service <- right $ mkService selfURL selfOrigins
    gateway <- right $ mkService gatewayURL ["http://ipgw.neu.edu.cn"]
    assert "callback must be allowlisted" $ isLeft $ mkService selfURL ["http://ipgw.neu.edu.cn"]
    assert "business whitelist excludes CAS" $ isLeft $ mkService selfURL ("https://pass.neu.edu.cn" : selfOrigins)
    assert "userinfo rejected" $ isLeft $ origin "https://u@ipgw.neu.edu.cn:8800/home"
    assert "invalid port rejected" $ isLeft $ origin "https://ipgw.neu.edu.cn:99999/home"
    assert "default port normalized" $ origin "https://pass.neu.edu.cn:443/x" == origin "https://pass.neu.edu.cn"
    assert "ports isolated" $ origin selfURL /= origin gatewayURL

    (session, finish) <- mock [page, js, home, ticket]
    landing <- authenticate session service "u😀" "p😀"
    assert "service may consume ticket and return home" $ landing == home
    second <- authenticate session gateway "unused" "unused"
    assert "existing CAS session can skip form for another service" $ second == ticket
    requests <- finish
    assert "no gateway activation inside CAS" $ length requests == 4
    assert "service URL encoded correctly" $ case requests of
        first : _ -> queryValue "service" (requestURL first) == Right selfURL
        _ -> False
    assert "different service requests a distinct callback" $ queryValue "service" (requestURL $ last requests) == Right gatewayURL
    assert "JavaScript UTF16 lengths" $ case requestForm (requests !! 2) of
        Just fields -> lookup "ul" fields == Just "3" && lookup "pl" fields == Just "3" && lookup "password" fields == Nothing
        Nothing -> False

    (bad, finishBad) <- mock [page, js, failed]
    casFailure "credential rejection / extra verification" CredentialsRejectedOrInteractionRequired $ authenticate bad service "u" "p"
    _ <- finishBad
    (detour, finishDetour) <- mock [page, js, email, home]
    _ <- authenticate detour service "u" "p"
    _ <- finishDetour
    (looping, finishLooping) <- mock [page, js, email, email]
    casFailure "detour retry is bounded" InteractionRequired $ authenticate looping service "u" "p"
    _ <- finishLooping
    (challenge, finishChallenge) <- mock [page, js, page {replyBody = "<title>MFA</title>"}]
    casFailure "unknown auth challenge needs interaction" InteractionRequired $ authenticate challenge service "u" "p"
    _ <- finishChallenge
    let hostile = page {replyBody = TE.encodeUtf8 $ T.replace "/tpass/login?service=fixture&amp;renew=true" "https://evil.example/login" html}
    (unsafe, finishUnsafe) <- mock [hostile]
    casFailure "credentials never posted to service or external host" (CASProtocolError "SSO 表单或脚本指向了不可信地址") $ authenticate unsafe service "u" "p"
    _ <- finishUnsafe
    let hostileScript = page {replyBody = TE.encodeUtf8 $ T.replace "/tpass/static/login_neu.js" "http://pass.neu.edu.cn/login_neu.js" html}
    (unsafeJS, finishJS) <- mock [hostileScript]
    casFailure "reject insecure public key script" (CASProtocolError "SSO 表单或脚本指向了不可信地址") $ authenticate unsafeJS service "u" "p"
    _ <- finishJS
    (redirectedJS, finishRedirectedJS) <- mock [page, home]
    serviceFailure "public key redirects must stay on CAS" $ authenticate redirectedJS service "u" "p"
    _ <- finishRedirectedJS
    (wrongPort, finishPort) <- mock [ticket]
    serviceFailure "service cannot land on gateway port" $ authenticate wrongPort service "u" "p"
    _ <- finishPort
    (noNetwork, finishNone) <- mock []
    serviceFailure "initial business request checked" $ requestService noNetwork service $ RequestSpec gatewayURL Nothing Nothing
    _ <- finishNone
    sessionTests

-- Exercise the actual HTTP transport, cookies and redirect boundary, without
-- campus hosts, credentials, environment proxies or a privileged listener.
sessionTests :: IO ()
sessionTests = do
    hits <- newIORef (0 :: Int)
    testWithApplication (pure $ target hits) $ \otherPort ->
        testWithApplication (pure $ app otherPort) $ \port -> do
            let base = "http://127.0.0.1:" <> T.pack (show port)
                get path = RequestSpec (base <> path) Nothing Nothing
            allowed <- (: []) <$> right (origin base)
            session <- newSession True 3
            first <- request session allowed $ get "/cookie"
            assert "redirect carries cookie" $ replyBody first == "cookie-ok"
            again <- request session allowed $ get "/check"
            assert "cookie reused on subsequent request" $ replyBody again == "cookie-ok"
            yii <- request session allowed $ get "/yii"
            assert "Yii X-Redirect follows same-origin destination" $ replyBody yii == "cookie-ok"
            other <- newSession True 3
            isolated <- request other allowed $ get "/check"
            assert "new session does not reuse cookies" $ replyBody isolated == "no-cookie"
            serviceFailure "redirect to different port blocked before request" $ request session allowed $ get "/escape"
            serviceFailure "Yii redirect cannot escape allowed origins" $ request session allowed $ get "/yii-escape"
            n <- readIORef hits
            assert "blocked host receives no request" $ n == 0
            mapM_ (\path -> serviceFailure "307/308 must not repost credentials" $ request session allowed (get path) {requestForm = Just [("rsa", "test-only")]}) ["/307", "/308", "/yii307"]
            result <- request session allowed (get "/303") {requestForm = Just [("rsa", "test-only")], requestReferer = Just base}
            assert "303 clears body and referer" $ replyBody result == "get-ok"
            serviceFailure "redirect loops bounded" $ request session allowed $ get "/loop"
            serviceFailure "HTTP error surfaced" $ request session allowed $ get "/error"
  where
    target hits _ respond = do
        modifyIORef' hits (+ 1)
        respond $ Wai.responseLBS status200 [] "unexpected"
    app otherPort req respond =
        let redirect status url headers = respond $ Wai.responseLBS status ((hLocation, url) : headers) ""
            ok body = respond $ Wai.responseLBS status200 [] body
         in case Wai.rawPathInfo req of
                "/cookie" -> redirect status302 "/check" [(hSetCookie, "session=fixture; Path=/; HttpOnly")]
                "/yii" -> respond $ Wai.responseLBS status302 [("X-Redirect", "/check")] ""
                "/yii-escape" -> respond $ Wai.responseLBS status302 [("X-Redirect", TE.encodeUtf8 $ "http://127.0.0.1:" <> T.pack (show otherPort) <> "/")] ""
                "/yii307" -> respond $ Wai.responseLBS status307 [("X-Redirect", "/method")] ""
                "/check" -> ok $ if maybe False (BS.isInfixOf "session=fixture") (lookup hCookie $ Wai.requestHeaders req) then "cookie-ok" else "no-cookie"
                "/escape" -> redirect status302 (TE.encodeUtf8 $ "http://127.0.0.1:" <> T.pack (show otherPort) <> "/") []
                "/307" -> redirect status307 "/method" []
                "/308" -> redirect status308 "/method" []
                "/303" -> redirect status303 "/method" []
                "/method" -> do
                    body <- Wai.strictRequestBody req
                    ok $ if Wai.requestMethod req == methodGet && body == "" && lookup hReferer (Wai.requestHeaders req) == Nothing then "get-ok" else "wrong-method"
                "/loop" -> redirect status302 "/loop" []
                _ -> respond $ Wai.responseLBS status503 [] "unavailable"
