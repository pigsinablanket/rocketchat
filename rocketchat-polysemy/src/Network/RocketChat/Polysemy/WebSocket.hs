module Network.RocketChat.Polysemy.WebSocket where

import           Data.UUID (UUID)
import           Network.RocketChat.Polysemy.Internal
import           Network.RocketChat.Polysemy.Config
import           Network.RocketChat.Types
import           Network.RocketChat.WebSocket as RC
import qualified Network.WebSockets as WS
import           Polysemy
import           Relude

data WebSocketE m a where
    SendRequest :: Request -> WebSocketE m ()
    Connect :: Request -> WebSocketE m ()
    Login :: Request -> WebSocketE m ()
    SendPing :: WebSocketE m ()
    GetRooms :: UUID -> WebSocketE m ()
    GetPublicSettings :: UUID -> WebSocketE m ()
    CloseConnection :: WebSocketE m ()
    RecieveMessage :: WebSocketE m Message

makeSem ''WebSocketE

runWebSocket :: Members '[Embed IO] r
             => WS.Connection -> Sem (WebSocketE : r) a -> Sem r a
runWebSocket conn = interpret $ \case
    SendRequest r -> embed $ RC.sendRequest conn r
    Connect r -> embed $ RC.connect conn r
    Login r -> embed $ RC.login conn r
    SendPing -> embed $ RC.sendPing conn
    GetRooms u -> embed $ RC.getRooms conn u
    GetPublicSettings u -> embed $ RC.getPublicSettings conn u
    CloseConnection -> embed $ RC.closeConnection conn
    RecieveMessage -> embed $ WS.receiveData conn

initializeWebSocket :: Members '[Final IO, Embed IO, ConfigE] r
                    => (WS.Connection -> Sem r ()) -> Sem r ()
initializeWebSocket app = do
    config <- getConfig
    ioApp <- bindSemToIO app
    embed_ $ startWebSocketConnection (cf_host config) (cf_port config) ioApp
