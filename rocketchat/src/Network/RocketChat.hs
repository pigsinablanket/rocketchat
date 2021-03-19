module Network.RocketChat
  ( module Network.RocketChat
  , module Network.RocketChat.Logging
  , module Network.RocketChat.Types
  , module Network.RocketChat.WebSocket
  ) where

import           Control.Concurrent (forkIO)
import qualified Network.WebSockets as WS
import           Relude
import qualified Data.UUID.V4 as UUID (nextRandom)

import           Network.RocketChat.Logging
import           Network.RocketChat.Types
import           Network.RocketChat.Config
import           Network.RocketChat.WebSocket

runRocketChat :: (RC_Instance -> Message -> IO ()) -> FilePath -> IO ()
runRocketChat handler cfgPath = do
    config <- readAndParseConfig cfgPath
    startWebSocketConnection (cf_host config) (cf_port config) (bot handler cfgPath)

bot :: (RC_Instance -> Message -> IO ()) -> FilePath -> WS.Connection -> IO ()
bot handler cfgPath conn = do
  config <- readAndParseConfig cfgPath
  connect conn defaultConnectRequest
  messageLoop config
  where
    messageLoop config = do
      message <- WS.receiveData conn
      debug message
      _ <- forkIO $ defaultHandler (rc_instance config) message
      _ <- forkIO $ handler (rc_instance config) message
      messageLoop config
    rc_instance = RC_Instance conn

-- | Default actions for handling responses
defaultHandler :: RC_Instance -> Message -> IO ()
defaultHandler (RC_Instance conn _) msg = do
  uuid <- UUID.nextRandom
  case message_type msg of
    Just Connected -> login conn $ login_request uuid
    Just Ping      -> sendPing conn
    _              -> return ()
  where
    login_request uuid = loginRequest {
        mr_id  = uuid
      , mr_params = [ Credentials
                      (Username "test-bot")
                      (encode_pass "password")
                    ] }
