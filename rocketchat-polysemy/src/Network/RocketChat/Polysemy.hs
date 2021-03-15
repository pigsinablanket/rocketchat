module Network.RocketChat.Polysemy
  ( module Network.RocketChat.Polysemy.Config
  , module Network.RocketChat.Polysemy.Logging
  , module Network.RocketChat.Polysemy.UUID
  , module Network.RocketChat.Polysemy.WebSocket
  , runRocketChat
  , defaultHandler
  , RocketChatE
  ) where

import Relude
import Polysemy
import Polysemy.Async
import Network.RocketChat.Types
import Network.RocketChat.Polysemy.Config
import Network.RocketChat.Polysemy.Logging
import Network.RocketChat.Polysemy.UUID
import Network.RocketChat.Polysemy.WebSocket

type RocketChatE = [WebSocketE,LoggingE,ConfigE,UUID,Async]

runRocketChat :: Members [WebSocketE,LoggingE,ConfigE,UUID,Async] r
              => (Message -> Sem r ()) -> FilePath -> IO ()
runRocketChat handler cfgPath =
    runFinal . embedToFinal . asyncToIOFinal . runLogging . (runConfig cfgPath) . runUUID  $ do
        initializeWebSocket (\conn -> runWebSocket conn (bot handler))

bot :: Members [WebSocketE,LoggingE,UUID,Async] r
    => (Message -> Sem x ()) -> Sem r ()
bot _handler = do
    connect defaultConnectRequest
    forever $ do
        message <- recieveMessage
        debug message
        async $ defaultHandler message

-- | Default actions for handling responses
defaultHandler :: Members [WebSocketE,LoggingE,UUID] r
                => Message -> Sem r ()
defaultHandler msg = do
    uuid <- genUUID
    case message_type msg of
        Just Connected -> login $ login_request uuid
        Just Ping -> getRooms uuid -- send_ping conn
        _ -> return ()
  where
    login_request uuid = loginRequest {
        mr_id  = uuid
      , mr_params = [ Credentials
                      (Username "test-bot")
                      (encode_pass "password")
                    ]
      }
