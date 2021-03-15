module Network.RocketChat.Monad.WebSocket where

import           Data.UUID (UUID)
import           Network.RocketChat.Types
import qualified Network.RocketChat.WebSocket as RC
import           Network.Socket (HostName, PortNumber)
import qualified Network.WebSockets as WS
import           Relude
import           UnliftIO

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
    sendRequest c r = liftIO $ RC.send_request c r
    connect c r = liftIO $ RC.connect c r
    login c r = liftIO $ RC.login c r
    sendPing c = liftIO $ RC.send_ping c
    getRooms c u = liftIO $ RC.get_rooms c u
    getPublicSettings c u = liftIO $ RC.get_public_settings c u
    closeConnection c = liftIO $ RC.close_connection c
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

initializeWebSocket :: MonadUnliftIO m => HostName -> PortNumber -> (WS.Connection -> m ()) -> m ()
initializeWebSocket hostname port app =
    withRunInIO $ \runInIO ->
        RC.startWebSocketConnection
            hostname
            port
            (runInIO . app)
