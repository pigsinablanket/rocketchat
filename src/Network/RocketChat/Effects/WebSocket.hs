module Network.RocketChat.Effects.WebSocket where

import Polysemy
import qualified Polysemy as P
import qualified Polysemy.Final as P
import           Relude
import           Network.Connection
import           Network.Socket            (HostName, PortNumber)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import qualified Data.ByteString.Lazy as BL (toStrict)
import           Data.UUID (UUID)
import Network.RocketChat.WebSocket as RC
import Network.RocketChat.Types

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
  ctx <- embed $ initConnectionContext
  con <- embed $ connectTo ctx $ ConnectionParams
                          { connectionHostname = hostname
                          , connectionPort = port
                          , connectionUseSecure =
                            Just $ TLSSettingsSimple
                              { settingDisableCertificateValidation = True
                              , settingDisableSession = False
                              , settingUseServerName = False
                              }
                          , connectionUseSocks = Nothing
                          }
  stream <- embed $ WS.makeStream (fmap Just $ connectionGetChunk con)
                          (maybe (return ()) (connectionPut con . BL.toStrict))
  _ <- embed $ WS.runClientWithStream stream hostname "/websocket" WS.defaultConnectionOptions [] app'
  pure ()

bindSemToIO :: P.Member (P.Final IO) r => (p -> Sem r a) -> Sem r (p -> IO (Maybe a))
bindSemToIO m = P.withStrategicToFinal $ do
  istate <- P.getInitialStateS
  m' <- P.bindS m
  ins <- P.getInspectorS
  P.liftS $ pure (\x -> P.inspect ins <$> m' (istate $> x))
