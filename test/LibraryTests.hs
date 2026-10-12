{-# LANGUAGE OverloadedStrings #-}

module LibraryTests (libraryTests) where

import Control.Monad (unless)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import NEU.HTTP.Session (RequestSpec (..))
import NEU.Library

libraryTests :: IO ()
libraryTests = do
    let html =
            "<table><tr><td>Haskell Basics author:Bird call:TP311.1 publisher:Press year:2022 ISBN:978-7-302-60827-1 copies:3 loaned:0</td></tr><tr><td>ignored</td></tr></table>"
    hits <- either (fail . T.unpack) pure $ parseCatalog 10 (TE.encodeUtf8 html)
    case hits of
        [hit] -> do
            assert "title" $ hitTitle hit == "Haskell Basics"
            assert "author" $ "Bird" `T.isInfixOf` hitAuthor hit
            assert "call" $ hitCallNumber hit == "TP311.1"
            assert "year" $ hitYear hit == "2022"
            assert "copies" $ hitCopies hit == "3" && hitLoaned hit == "0"
        _ -> fail $ "unexpected hits: " ++ show (map hitTitle hits)
    let page = "http://www.lib.neu.edu.cn/page/ggxx/tsg"
        newsHTML = "<a href=\"/page/ggxx/tsg/2026/1012/1766.html\">library resource survey notice</a><a href=\"/short\">short</a>"
    items <- either (fail . T.unpack) pure $ parseNews page (TE.encodeUtf8 newsHTML)
    case items of
        [item] -> assert "news date" $ newsDate item == "2026-10-12" && "survey" `T.isInfixOf` newsTitle item
        _ -> fail $ "unexpected news: " ++ show (map newsTitle items)
    let request = catalogRequest "http://210.30.202.58:8991/" (CatalogQuery "haskell" "WRD" "NEU01" 10)
    assert "catalog request" $ "func=find-b" `T.isInfixOf` requestURL request && "210.30.202.58:8991/F" `T.isInfixOf` requestURL request
    putStrLn "Library catalog and news parsing checks passed."
  where
    assert label condition = unless condition $ fail label
