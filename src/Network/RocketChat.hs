-- |
-- Module      : Network.RocketChat
-- Copyright   : (c) 2021 Daniel Reimer
-- License     : MIT
-- Maintainer  : Daniel Reimer <daniel.k.reimer97@gmail.com>
-- Stability   : experimental
--
-- RocketChat library.

module Network.RocketChat
  ( module Network.RocketChat
  , module Network.RocketChat.Logging
  , module Network.RocketChat.Types
  , module Network.RocketChat.WebSocket
  , module Network.RocketChat.Effects
  ) where

import           Crypto.Hash.SHA256 (hash)
import qualified Data.Aeson as A
import qualified Data.ByteString.Base16 as BS (encode)
import qualified Data.HashMap.Strict as HM (toList)
import qualified Data.List as L (lookup)
import qualified Data.Text as T (Text)
import qualified Network.WebSockets as WS
import           Polysemy
import           Polysemy.Async
import           Relude

import           Network.RocketChat.Logging
import           Network.RocketChat.Types
import           Network.RocketChat.WebSocket (listen_for_uuid)
import           Network.RocketChat.Effects

runRocketChat :: Members [WebSocketE,LoggingE,ConfigE,UUID] r => (RC_Instance -> Message -> Sem r ()) -> FilePath -> IO ()
runRocketChat handler cfgPath = do
  runFinal . embedToFinal . asyncToIO . runLogging . runConfig . runUUID . runWebSocket $ do
    config <- getConfig cfgPath
    initializeWebSocket (cf_host config) (cf_port config) (bot handler cfgPath)

bot :: Members [WebSocketE,UUID,LoggingE,ConfigE,Async] r
    => (RC_Instance -> Message -> Sem x ()) -> FilePath -> WS.Connection -> Sem r ()
bot _handler cfgPath conn = do
  config <- getConfig cfgPath
  connect conn defaultConnectRequest
  messageLoop config
  where
    messageLoop config = do
      message <- recieveMessage conn
      debug message
      _ <- async $ default_handler (rc_instance config) message
      messageLoop config
    rc_instance = RC_Instance conn

-- | Default actions for handling responses
default_handler :: Members [WebSocketE,UUID,LoggingE,ConfigE] r
                => RC_Instance -> Message -> Sem r ()
default_handler (RC_Instance conn _) msg = do
  uuid <- genUUID
  case message_type msg of
    Just Connected -> login conn $ login_request uuid
    Just Ping      -> getRooms conn uuid -- send_ping conn
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
