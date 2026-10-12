{-# LANGUAGE OverloadedStrings #-}

-- Read-only NEU library operations. Catalog search uses the Aleph OPAC at
-- 210.30.202.58:8991; news comes from the public library site.
module NEU.Library (
    CatalogQuery (..),
    CatalogHit (..),
    NewsItem (..),
    catalogRequest,
    parseCatalog,
    newsRequest,
    parseNews,
    renderHits,
    renderNews,
    hitsJSON,
    newsJSON,
) where

import Data.Aeson (Value, object, (.=))
import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.List (find, tails)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import NEU.HTTP.Session (RequestSpec (..))
import NEU.HTTP.URL (resolveURL, urlWith)
import NEU.IPGW.HTML (safe)
import Text.HTML.TagSoup (Tag (..), innerText, parseTags)

data CatalogQuery = CatalogQuery
    { queryText :: Text
    , queryField :: Text
    , queryBase :: Text
    , queryLimit :: Int
    }
    deriving (Eq, Show)

data CatalogHit = CatalogHit
    { hitTitle :: Text
    , hitAuthor :: Text
    , hitCallNumber :: Text
    , hitPublisher :: Text
    , hitYear :: Text
    , hitISBN :: Text
    , hitCopies :: Text
    , hitLoaned :: Text
    }
    deriving (Eq, Show)

data NewsItem = NewsItem {newsTitle :: Text, newsURL :: Text, newsDate :: Text} deriving (Eq, Show)

newsPages :: [(Text, Text)]
newsPages = [("notice", "/page/ggxx/tsg"), ("resource", "/page/zydt/dt")]

catalogRequest :: Text -> CatalogQuery -> RequestSpec
catalogRequest base CatalogQuery {queryText = text, queryField = field, queryBase = localBase} =
    RequestSpec
        ( urlWith
            (stripSlash base <> "/F")
            [ ("func", "find-b")
            , ("find_code", TE.encodeUtf8 field)
            , ("request", TE.encodeUtf8 text)
            , ("local_base", TE.encodeUtf8 localBase)
            ]
        )
        Nothing
        Nothing
  where
    stripSlash value = fromMaybeEnd value
    fromMaybeEnd value = if T.isSuffixOf "/" value then T.init value else value

newsRequest :: Text -> Text -> RequestSpec
newsRequest base kind = RequestSpec (stripSlash base <> path) Nothing Nothing
  where
    path = maybe "/page/ggxx/tsg" id $ lookup kind newsPages
    stripSlash value = if T.isSuffixOf "/" value then T.init value else value

parseCatalog :: Int -> BS.ByteString -> Either Text [CatalogHit]
parseCatalog limit body = do
    let html = TE.decodeUtf8With lenientDecode body
        tags = parseTags html
    if T.null (T.strip html) then Left "馆藏响应为空" else Right ()
    let rows = take limit $ filter (not . T.null . hitAuthor) $ map parseRow $ recordGroups tags
    Right rows
  where
    recordGroups tags =
        let items = blocks isItem tags
         in if null items then blocks isRow tags else items
    blocks marker = go
      where
        go [] = []
        go (tag : rest)
            | marker tag = let (block, more) = break marker rest in (tag : block) : go more
            | otherwise = go rest
    isItem (TagOpen "div" attrs) = lookup "class" attrs == Just "itemtitle"
    isItem _ = False
    isRow (TagOpen "tr" _) = True
    isRow _ = False
    parseRow row =
        let text = T.unwords $ scriptTexts row
            quoted = scriptQuotes row
            visible = beforeAny ["作者：", "author:"] text
            title = T.strip $ fst $ T.breakOn "vpn_eval" $ if T.null visible then maybe "" id (find isTitle quoted) else visible
         in CatalogHit
                { hitTitle = title
                , hitAuthor = T.stripEnd $ firstPresent [between "作者：" "索书号：" text, between "author:" "call:" text]
                , hitCallNumber = firstPresent [between "索书号：" "出版社：" text, between "call:" "publisher:" text]
                , hitPublisher = firstPresent [between "出版社：" "年份：" text, between "publisher:" "year:" text]
                , hitYear = T.takeWhile isDigit $ T.strip $ firstPresent [labeled "年份：" row, after "year:" text]
                , hitISBN = T.takeWhile (\c -> isDigit c || c == '-') $ T.dropWhile (not . isDigit) $ firstPresent [after "fmt_issn" text, after "ISBN:" text]
                , hitCopies = firstPresent [afterLabel "馆藏复本:" text, afterLabel "copies:" text]
                , hitLoaned = firstPresent [afterLabel "已出借复本:" text, afterLabel "loaned:" text]
                }
    scriptTexts row = [innerText [tag] | tag@(TagText _) <- row]
    scriptQuotes row = [value | TagText script <- row, value <- quotedStrings script]
    labeled label row = case dropWhile (\tag -> not (isLabel label tag)) row of
        _ : rest -> T.strip $ innerText $ takeWhile (\tag -> tag /= TagClose "td") rest
        [] -> ""
    isLabel label (TagText value) = T.strip value == label
    isLabel _ _ = False
    firstPresent values = maybe "" id $ find (not . T.null) values
    quotedStrings script = case T.breakOn "(\"" script of
        (_, "") -> []
        (_, rest) -> case T.breakOn "\")" (T.drop 2 rest) of
            (value, "") -> [value]
            (value, more) -> value : quotedStrings (T.drop 2 more)
    isTitle value = T.length value > 1 && T.all (\c -> c /= '{' && c /= '}' && c /= '(' && c /= ';') value && not (T.isInfixOf "http" value) && not (T.isInfixOf "function" value)
    between start end value = T.strip $ T.takeWhile (const True) $ before end $ after start value
    beforeAny markers value = T.strip $ foldl (\current marker -> fst $ T.breakOn marker current) value markers
    before marker value = case T.breakOn marker value of (prefix, _) -> prefix
    after marker value = case T.breakOn marker value of (_, rest) -> maybe rest id $ T.stripPrefix marker rest
    afterLabel marker value = T.takeWhile isDigit $ T.dropWhile (not . isDigit) $ after marker value

parseNews :: Text -> BS.ByteString -> Either Text [NewsItem]
parseNews pageURL body = do
    let html = TE.decodeUtf8With lenientDecode body
        tags = parseTags html
        links =
            [ NewsItem title url date
            | (TagOpen "a" attrs, rest) <- zip tags (drop 1 (tails tags))
            , let href = maybe "" id $ lookup "href" attrs
            , not (T.null href)
            , Right url <- [resolveURL pageURL href]
            , let title = safe $ T.strip $ innerText $ takeWhile (/= TagClose "a") rest
            , T.length title >= 8
            , let date = dateFrom url
            ]
    Right $ unique links
  where
    dateFrom url = case reverse $ filter (not . T.null) $ T.splitOn "/" url of
        _ : mmdd : year : _
            | allDigits year && T.length mmdd == 4 && allDigits mmdd ->
                year <> "-" <> T.take 2 mmdd <> "-" <> T.drop 2 mmdd
        _ -> ""
    allDigits value = not (T.null value) && T.all isDigit value
    unique [] = []
    unique (item : rest) = item : unique (filter ((/= newsURL item) . newsURL) rest)

renderHits :: [CatalogHit] -> Text
renderHits [] = "没有找到馆藏"
renderHits hits = T.intercalate "\n" $ zipWith render [1 :: Int ..] hits
  where
    render n hit =
        T.unlines
            [ T.pack (show n) <> ". " <> safe (hitTitle hit)
            , "作者：" <> dash (hitAuthor hit)
            , "索书号：" <> dash (hitCallNumber hit)
            , "出版：" <> T.intercalate "，" (filter (not . T.null) [safe (hitPublisher hit), safe (hitYear hit)])
            , "ISBN：" <> dash (hitISBN hit)
            , "复本：" <> dash (hitCopies hit) <> "，已借：" <> dash (hitLoaned hit)
            ]
    dash value = if T.null value then "—" else safe value

renderNews :: [NewsItem] -> Text
renderNews [] = "没有公告"
renderNews items = T.unlines [date <> safe (newsTitle item) | item <- items, let date = if T.null (newsDate item) then "" else newsDate item <> " "]

hitsJSON :: Text -> [CatalogHit] -> Value
hitsJSON route hits =
    object
        [ "route" .= route
        , "count" .= length hits
        , "hits"
            .= map
                ( \hit ->
                    object
                        [ "title" .= hitTitle hit
                        , "author" .= hitAuthor hit
                        , "call_number" .= hitCallNumber hit
                        , "publisher" .= hitPublisher hit
                        , "year" .= hitYear hit
                        , "isbn" .= hitISBN hit
                        , "copies" .= hitCopies hit
                        , "loaned" .= hitLoaned hit
                        ]
                )
                hits
        ]

newsJSON :: Text -> Text -> [NewsItem] -> Value
newsJSON route kind items =
    object
        [ "route" .= route
        , "kind" .= kind
        , "count" .= length items
        , "items" .= map (\item -> object ["title" .= newsTitle item, "url" .= newsURL item, "date" .= newsDate item]) items
        ]
