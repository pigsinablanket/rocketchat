module Network.RocketChat.Monad.WebSocket where

import           Data.UUID (UUID)
import           Network.RocketChat.Types
import qualified Network.RocketChat.WebSocket as RC
import qualified Network.WebSockets as WS
import           Relude
import           UnliftIO
import           Network.RocketChat.Monad.Config

class Monad m => MonadWebSocket m where
    sendRequest :: WS.Connection -> Request -> m ()
    connect :: WS.Connection -> Request -> m ()
    login :: WS.Connection -> Request -> m ()
    sendPing :: WS.Connection -> m ()
    getRooms :: WS.Connection -> UUID -> m ()
    getPublicSettings :: WS.Connection -> UUID -> m ()
    closeConnection :: WS.Connection -> m ()
    recieveMessage :: WS.Connection -> m Message

instance MonadWebSocket IO where
    sendRequest c r = liftIO $ RC.sendRequest c r
    connect c r = liftIO $ RC.connect c r
    login c r = liftIO $ RC.login c r
    sendPing c = liftIO $ RC.sendPing c
    getRooms c u = liftIO $ RC.getRooms c u
    getPublicSettings c u = liftIO $ RC.getPublicSettings c u
    closeConnection c = liftIO $ RC.closeConnection c
    recieveMessage conn = liftIO $ WS.receiveData conn

instance MonadWebSocket m => MonadWebSocket (ReaderT r m) where
    sendRequest c r = lift (sendRequest c r)
    connect c r = lift (connect c r)
    login c r = lift (login c r)
    sendPing c = lift (sendPing c)
    getRooms c u = lift (getRooms c u)
    getPublicSettings c u = lift (getPublicSettings c u)
    closeConnection c = lift (closeConnection c)
    recieveMessage c = lift (recieveMessage c)

initializeWebSocket :: (MonadConfig m, MonadUnliftIO m)
                    => FilePath -> (WS.Connection -> m b) -> m b
initializeWebSocket cfgPath app = do
    config <- getConfig cfgPath
    withRunInIO $ \runInIO ->
        RC.startWebSocketConnection
            (hostname config)
            (port config)
            (runInIO . app)
  where
    hostname = cf_host
    port = cf_port
