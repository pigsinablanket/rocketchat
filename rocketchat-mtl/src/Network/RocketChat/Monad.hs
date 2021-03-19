{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Network.RocketChat.Monad where

import Relude
import Network.RocketChat.Types
import Network.RocketChat.Monad.UUID
import Network.RocketChat.Monad.Config
import Network.RocketChat.Monad.WebSocket
import Network.RocketChat.Monad.Logging
import qualified Network.WebSockets as WS
import UnliftIO
import Control.Concurrent

newtype RocketChatT m a = RocketChatT { unRocketChatT :: ReaderT () m a }
    deriving ( Monad
             , Applicative
             , Functor
             , MonadReader ()
             , MonadIO
             , MonadTrans
             , MonadUnliftIO
             )

deriving instance MonadUUID m => MonadUUID (RocketChatT m)
deriving instance MonadConfig m => MonadConfig (RocketChatT m)
deriving instance MonadWebSocket m => MonadWebSocket (RocketChatT m)
deriving instance MonadLogging m => MonadLogging (RocketChatT m)

type MonadRocketChat m = (MonadUUID m,MonadConfig m, MonadWebSocket m, MonadLogging m, MonadUnliftIO m, MonadIO m)

runRocketChatT :: RocketChatT m a -> m a
runRocketChatT op = usingReaderT () (unRocketChatT op)

runRocketChat :: (MonadUUID m,MonadConfig m, MonadWebSocket m, MonadLogging m, MonadUnliftIO m, MonadIO m)
              => (Message -> m ()) -> FilePath -> IO ()
runRocketChat handler cfgPath = runRocketChatT $
    initializeWebSocket cfgPath (bot handler)

bot :: (MonadUUID m,MonadWebSocket m, MonadIO m,MonadLogging m)
    => (Message -> p ()) -> WS.Connection -> m ()
bot _handler conn = do
    connect conn defaultConnectRequest
    debug "connected"
    forever $ do
        message <- recieveMessage conn
        debug message
        liftIO $ forkIO $ defaultHandler message conn

defaultHandler :: (MonadUUID m, MonadWebSocket m, MonadLogging m)
               => Message -> WS.Connection -> m ()
defaultHandler msg conn = do
    uuid <- genUUID
    case message_type msg of
        Just Connected -> login conn $ login_request uuid
        Just Ping -> sendPing conn
        _ -> return ()
  where
    login_request uuid = loginRequest {
        mr_id  = uuid
      , mr_params = [ Credentials
                      (Username "test-bot")
                      (encode_pass "password")
                    ] }
