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
  ( module Network.RocketChat
  , module Network.RocketChat.Config
  , module Network.RocketChat.Logging
  , module Network.RocketChat.Types
  , module Network.RocketChat.WebSocket
  ) where

import           Control.Concurrent        (forkIO)
import           Crypto.Hash.SHA256        (hash)
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as BL (toStrict)
import qualified Data.ByteString.Base16    as BS (encode)
import qualified Data.HashMap.Strict       as HM (toList)
import qualified Data.List                 as L (lookup)
import qualified Data.Text                 as T (Text)
import qualified Data.UUID.V4              as UUID (nextRandom)
import           Network.Connection
import           Network.Socket            (HostName, PortNumber)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import           Relude

import           Network.RocketChat.Config
import           Network.RocketChat.Logging
import           Network.RocketChat.Types
import           Network.RocketChat.WebSocket

run :: Handler -> FilePath -> IO ()
run handler cfg_path = do
  config <- parse_config cfg_path
  putStrLn $ "Config parsed"
  initialize (bot handler config) (cf_host config) (cf_port config)

-- | Starts the connection to the websocket
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

bot :: Handler -> Config -> WS.ClientApp ()
bot handler config conn = do
  connect conn defaultConnectRequest
  forever $ do
      message <- WS.receiveData conn
      log_msg_recv message
      forkIO $ handler rc_instance message
  where
    rc_instance = RC_Instance conn config

-- | Default actions for handling responses
default_handler :: RC_Instance -> Message -> IO ()
default_handler (RC_Instance conn _) msg = do
  uuid <- gen_uuid
  case message_type msg of
    Just Connected -> login conn $ login_request uuid
    Just Ping      -> get_rooms conn uuid -- send_ping conn
    _              -> return ()
  where
    login_request uuid = loginRequest {
        mr_id  = uuid
      , mr_params = [ Credentials
                      (Username "test-bot")
                      (encode_pass "password")
                    ] }

-- | Encodes a password with sha-256
encode_pass :: T.Text -> Password
encode_pass pwd = Password (gen_digest pwd) "sha-256"
  where
    gen_digest = decodeUtf8 . BS.encode . hash . encodeUtf8

-- | Returns a randomly generated UUID
gen_uuid :: IO UUID
gen_uuid = UUID.nextRandom

-- | Retrieves the type of message received
message_type :: Message -> Maybe MessageResponse
message_type msg = case (A.decodeStrict (encodeUtf8 msg)) :: Maybe A.Value of
                   Just x  -> msg_field x
                   Nothing -> Nothing
  where
    msg_field :: A.Value -> Maybe MessageResponse
    msg_field (A.Object o) = case L.lookup "msg" (HM.toList o) of
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
