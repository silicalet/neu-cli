{-# LANGUAGE OverloadedStrings #-}

module NEU.IPGW.Config (
    Account (..),
    Config (..),
    emptyConfig,
    configPath,
    loadConfig,
    saveConfig,
    putAccount,
    setDefault,
    selectAccount,
) where

import Control.Exception (IOException, bracketOnError, catch, throwIO)
import Control.Monad (unless)
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Text (Text)
import NEU.IPGW.Client (IPGWError (..))
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory, removeFile, renameFile)
import System.FilePath (takeDirectory)
import System.IO (hClose, openBinaryTempFile)
import System.Posix.Files (setFileMode)

data Account = Account {accountUsername :: Text, accountPassword :: Text} deriving (Eq)
data Config = Config {accounts :: [Account], defaultUsername :: Maybe Text} deriving (Eq)

instance FromJSON Account where
    parseJSON = withObject "account" $ \o -> Account <$> o .: "username" <*> o .: "password"
instance ToJSON Account where
    toJSON a = object ["username" .= accountUsername a, "password" .= accountPassword a]
instance FromJSON Config where
    parseJSON = withObject "config" $ \o -> do
        version <- o .: "version"
        unless (version == (1 :: Int)) $ fail "unsupported config version"
        Config <$> o .: "accounts" <*> o .:? "default_username"
instance ToJSON Config where
    toJSON c = object ["version" .= (1 :: Int), "accounts" .= accounts c, "default_username" .= defaultUsername c]

emptyConfig :: Config
emptyConfig = Config [] Nothing

configPath :: IO FilePath
configPath = getXdgDirectory XdgConfig "neu/ipgw.json"

loadConfig :: FilePath -> IO Config
loadConfig path = do
    exists <- doesFileExist path
    if not exists
        then pure emptyConfig
        else do
            content <- BL.readFile path
            either (const $ throwIO $ IPGWError "配置文件无效；未修改原文件") pure (eitherDecode content)

-- Write a private temporary file then atomically replace the configuration.
saveConfig :: FilePath -> Config -> IO ()
saveConfig path config = do
    let directory = takeDirectory path
    createDirectoryIfMissing True directory
    bracketOnError
        (openBinaryTempFile directory ".ipgw.json.tmp")
        ( \(tmp, handle) -> do
            hClose handle `catch` ignore
            removeFile tmp `catch` ignore
        )
        ( \(tmp, handle) -> do
            setFileMode tmp 0o600
            BL.hPut handle $ encode config
            hClose handle
            renameFile tmp path
        )
  where
    ignore :: IOException -> IO ()
    ignore _ = pure ()

putAccount :: Account -> Config -> Config
putAccount account config =
    config
        { accounts = account : filter ((/= accountUsername account) . accountUsername) (accounts config)
        }

setDefault :: Text -> Config -> Either Text Config
setDefault username config = case find ((== username) . accountUsername) (accounts config) of
    Nothing -> Left "账号尚未添加；请先使用 neu ipgw add -u 学工号"
    Just _ -> Right config {defaultUsername = Just username}

selectAccount :: Maybe Text -> Config -> Either Text Account
selectAccount username config = do
    name <- maybe (Left "未指定账号，也没有默认账号；请使用 -u 或 neu ipgw default") Right $
        case username of Just u -> Just u; Nothing -> defaultUsername config
    maybe (Left "账号尚未保存；请使用 neu ipgw add，或指定 -u 并输入密码") Right $
        find ((== name) . accountUsername) (accounts config)
