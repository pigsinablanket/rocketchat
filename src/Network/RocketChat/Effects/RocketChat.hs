module Network.RocketChat.Effects.RocketChat where

import Network.RocketChat.Types
import Network.RocketChat.WebSocket as RC
import Data.UUID (UUID)
import           Polysemy
import           Relude
import Network.WebSockets (Connection)
import qualified Network.WebSockets        as WS

data RocketChatE m a where
  SendRequest :: Connection -> Request -> RocketChatE m ()
  Connect :: Connection -> Request -> RocketChatE m ()
  Login :: Connection -> Request -> RocketChatE m ()
  SendPing :: Connection -> RocketChatE m ()
  GetRooms :: Connection -> UUID -> RocketChatE m ()
  GetPublicSettings :: Connection -> UUID -> RocketChatE m ()
  CloseConnection :: Connection -> RocketChatE m ()
  RecieveMessage :: WS.Connection -> RocketChatE m Message

makeSem ''RocketChatE

runRocketChat :: Members '[Embed IO] r => Sem (RocketChatE : r) a -> Sem r a
runRocketChat = interpret $ \case
  SendRequest c r -> embed $ RC.send_request c r
  Connect c r -> embed $ RC.connect c r
  Login c r -> embed $ RC.login c r
  SendPing c -> embed $ RC.send_ping c
  GetRooms c u -> embed $ RC.get_rooms c u
  GetPublicSettings c u -> embed $ RC.get_public_settings c u
  CloseConnection c -> embed $ RC.close_connection c
  RecieveMessage conn -> embed $ WS.receiveData conn
