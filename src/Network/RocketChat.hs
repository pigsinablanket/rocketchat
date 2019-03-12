{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.Rocketchat
-- Copyright   : (c) 2019 Daniel Reimer
-- License     : MIT
-- Maintainer  : Daniel Reimer <daniel.k.reimer97@gmail.com>
-- Stability   : experimental
-- Portability : OverloadedStrings
--
-- RocketChat library.

module Network.RocketChat
  ( module Network.RocketChat.Types
  , module Network.RocketChat
  ) where

import           Crypto.Hash.SHA256        (hash)
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BL (toStrict)
import qualified Data.ByteString.Base16    as BS (encode)
import           Data.Maybe                (maybe)
import qualified Data.HashMap.Strict       as HM (toList)
import qualified Data.Text                 as T (Text)
import           Data.Text.Encoding        (decodeUtf8, encodeUtf8)
import           Network.Connection
import           Network.Socket            (HostName, PortNumber)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import           Network.RocketChat.Types

-- |
-- Starts the connection to the websocket
initialize :: WS.ClientApp b -> HostName -> PortNumber -> IO b
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

-- |
-- Sends a connection request to the websocket
connect :: WS.Connection -> ConnectRequest -> IO ()
connect conn connectRequest = do
  WS.sendTextData conn $ A.encode connectRequest

-- |
-- Sends a login request to the websocket
login :: WS.Connection -> LoginRequest -> IO ()
login conn loginRequest = do
  WS.sendTextData conn $ A.encode loginRequest

-- |
-- Encodes a password with sha-256
encode_pass :: T.Text -> Password
encode_pass pwd = Password (gen_digest pwd) "sha-256"
  where
    gen_digest = decodeUtf8 . BS.encode . hash . encodeUtf8

-- |
-- Retrieves the type of message recieved
message_type :: T.Text -> Maybe Message
message_type msg = case (A.decodeStrict (encodeUtf8 msg)) :: Maybe A.Value of
                   Just x  -> msg_field x
                   Nothing -> Nothing
  where
    msg_field :: A.Value -> Maybe Message
    msg_field (A.Object o) = case lookup "msg" (HM.toList o) of
                               Just x  -> msg_field x
                               Nothing -> Nothing
    msg_field (A.String s)
      | s == "added"     = Just Added
      | s == "changed"   = Just Changed
      | s == "connected" = Just Connected
      | s == "ping"      = Just Ping
      | s == "ready"     = Just Ready
      | s == "result"    = Just Result
      | s == "updated"   = Just Updated
      | otherwise       = Nothing
    msg_field _            = Nothing

-- |
-- Sends a ping to the websocket
send_ping :: WS.Connection -> IO ()
send_ping conn = WS.sendTextData conn $ A.encode pong
