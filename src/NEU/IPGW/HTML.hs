{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.HTML (
    elements,
    clean,
    hasClass,
    col,
    natural,
    decimal,
    timestamp,
    timeText,
    money,
    safe,
) where

import Control.Monad (unless)
import Data.Char (isControl)
import Data.Scientific (FPFormat (Fixed), Scientific, base10Exponent, formatScientific)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime, defaultTimeLocale, formatTime, parseTimeM)
import Text.HTML.TagSoup (Tag (..), innerText)
import Text.Read (readMaybe)

-- Scan all matching openings, preserving the complete body even when layout
-- elements of the same name nest. Consumers select by labels/classes, not order.
elements :: Text -> [Tag Text] -> [([(Text, Text)], [Tag Text])]
elements name = go
  where
    go [] = []
    go (TagOpen n attrs : rest)
        | n == name = (attrs, inside (0 :: Int) rest) : go rest
    go (_ : rest) = go rest
    inside _ [] = []
    inside depth (tag : rest) = case tag of
        TagClose n | n == name -> if depth == 0 then [] else tag : inside (depth - 1) rest
        TagOpen n _ | n == name -> tag : inside (depth + 1) rest
        _ -> tag : inside depth rest

clean :: [Tag Text] -> Text
clean = T.strip . innerText

hasClass :: Text -> [(Text, Text)] -> Bool
hasClass name attrs = maybe False (elem name . T.words) $ lookup "class" attrs

col :: [(Text, Text)] -> Text
col = maybe "" id . lookup "data-col-seq"

natural :: Text -> Either Text Integer
natural t
    | not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t = maybe invalid Right $ readMaybe $ T.unpack t
    | otherwise = invalid
  where
    invalid = Left "日志原始计数不是非负整数"

decimal :: Text -> Either Text Scientific
decimal t = do
    let unsigned = maybe t id $ T.stripPrefix "-" t
        parts = T.splitOn "." unsigned
    unless (length parts <= 2 && all (\p -> not (T.null p) && T.all (\c -> c >= '0' && c <= '9') p) parts) $ Left "日志金额格式无效"
    maybe (Left "日志金额格式无效") Right $ readMaybe $ T.unpack t

timestamp :: Text -> Either Text LocalTime
timestamp text = case [t | fmt <- ["%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M"], Just t <- [parseTimeM True defaultTimeLocale fmt (T.unpack text)]] of
    t : _ -> Right t
    [] -> Left "日志日期格式无效"

timeText :: LocalTime -> Text
timeText = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

money :: Scientific -> Text
money n = T.pack $ formatScientific Fixed (Just $ max 2 $ negate $ base10Exponent n) n

safe :: Text -> Text
safe = T.map (\c -> if isControl c || c == '|' then ' ' else c)
