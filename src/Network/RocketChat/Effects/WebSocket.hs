module Network.RocketChat.Effects.WebSocket where

import           Data.UUID (UUID)
import           Network.RocketChat.Internal
import           Network.RocketChat.Types
import           Network.RocketChat.WebSocket as RC
import           Network.Socket (HostName, PortNumber)
import qualified Network.WebSockets as WS
import           Polysemy
import           Relude

data WebSocketE m a where
    SendRequest :: WS.Connection -> Request -> WebSocketE m ()
    Connect :: WS.Connection -> Request -> WebSocketE m ()
    Login :: WS.Connection -> Request -> WebSocketE m ()
    SendPing :: WS.Connection -> WebSocketE m ()
    GetRooms :: WS.Connection -> UUID -> WebSocketE m ()
    GetPublicSettings :: WS.Connection -> UUID -> WebSocketE m ()
    CloseConnection :: WS.Connection -> WebSocketE m ()
    RecieveMessage :: WS.Connection -> WebSocketE m Message

makeSem ''WebSocketE

runWebSocket :: Members '[Embed IO] r
             => Sem (WebSocketE : r) a -> Sem r a
runWebSocket = interpret $ \case
    SendRequest c r -> embed $ RC.send_request c r
    Connect c r -> embed $ RC.connect c r
    Login c r -> embed $ RC.login c r
    SendPing c -> embed $ RC.send_ping c
    GetRooms c u -> embed $ RC.get_rooms c u
    GetPublicSettings c u -> embed $ RC.get_public_settings c u
    CloseConnection c -> embed $ RC.close_connection c
    RecieveMessage conn -> embed $ WS.receiveData conn

initializeWebSocket :: Members '[Final IO, Embed IO] r
                    => HostName -> PortNumber -> (WS.Connection -> Sem r ()) -> Sem r ()
initializeWebSocket hostname port app = do
  app' <- bindSemToIO app
  embed_ $ startWebSocketConnection hostname port app'
