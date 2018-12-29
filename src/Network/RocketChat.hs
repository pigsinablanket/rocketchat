{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Rocketchat
-- Copyright   : (c) 2018 Daniel Reimer
-- License     : MIT
-- Maintainer  : Daniel Reimer <daniel.k.reimer97@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- RocketChat library.

module Network.RocketChat
  ( -- *Type synonyms
    Hostname
  , Port
  , initialize
  , connect
  , defaultConnectRequest
  ) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (forever, unless)
import           Data.Aeson                (ToJSON, encode)
import           Data.Maybe                (maybe)
import           Data.Text                 (Text)
import           Network.Connection
import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS

import Network.RocketChat.Types

initialize app hostname port = do
  ctx <- initConnectionContext
  con <- connectTo ctx $ ConnectionParams
                          { connectionHostname  = hostname
                          , connectionPort      = port
                          , connectionUseSecure =
                            Just $ TLSSettingsSimple
                              { settingDisableCertificateValidation = True
                              , settingDisableSession = False
                              , settingUseServerName  = False
                              }
                          , connectionUseSocks  = Nothing
                          }
  stream <- WS.makeStream (fmap Just $ connectionGetChunk con)
                          (maybe (return ()) (connectionPut con . BL.toStrict))
  WS.runClientWithStream stream hostname "/websocket" WS.defaultConnectionOptions [] app

connect :: ToJSON a => WS.Connection -> a -> IO ()
connect conn connectRequest = do
  WS.sendTextData conn $ encode connectRequest
