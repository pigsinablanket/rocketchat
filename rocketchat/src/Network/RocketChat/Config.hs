{-# LANGUAGE OverloadedStrings #-}

module Network.RocketChat.Config
    ( readAndParseConfig
    ) where

import           Data.Ini.Config as INI
import qualified Data.Text.IO    as T (readFile)
import           Network.RocketChat.Types
import           Relude

configParser :: IniParser Config
configParser = do
  section "default" $ do
    host     <- fieldOf "host" string
    port     <- fieldOf "port" number
    username <- fieldOf "username" string
    password <- fieldOf "password" string
    return Config { cf_host = host
                  , cf_port = port
                  , cf_username = username
                  , cf_password = password }

readAndParseConfig :: FilePath -> IO Config
readAndParseConfig cfg_path = do
  cfg <- T.readFile cfg_path
  case INI.parseIniFile (add_default cfg) configParser of
    Left err_str -> error $ toText err_str
    Right config -> return config
  where
    add_default config = config
