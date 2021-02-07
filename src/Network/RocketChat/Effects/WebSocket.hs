module Network.RocketChat.Effects.WebSocket where

import Polysemy
import qualified Polysemy as P
import qualified Polysemy.Final as P
import           Relude
import           Network.Connection
import           Network.Socket            (HostName, PortNumber)
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS
import qualified Data.ByteString.Lazy      as BL (toStrict)

data WebSocketE m a where
  Initialize :: HostName -> PortNumber -> WebSocketE m ()

makeSem ''WebSocketE

runWebSocket :: Members '[Final IO, Embed IO] r
             => (WS.Connection -> Sem r a) -> Sem (WebSocketE : r) a -> Sem r a
runWebSocket app = interpret $ \case
  Initialize hostname port -> do
      app' <- bindSemToIO app
      _ <- embed $ initialize' hostname port app'
      pure ()

-- | Starts the connection to the websocket
-- initialize' :: Members [RocketChatE,LoggingE,ConfigE] r
--             => (Connection -> Sem r a) -> HostName -> PortNumber -> Sem r (Maybe a)
initialize' :: HostName -> PortNumber -> WS.ClientApp b -> IO b
initialize' hostname port app = do
  ctx <- initConnectionContext
  con <- connectTo ctx $ ConnectionParams
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
  stream <- WS.makeStream (fmap Just $ connectionGetChunk con)
                          (maybe (return ()) (connectionPut con . BL.toStrict))
  WS.runClientWithStream stream hostname "/websocket" WS.defaultConnectionOptions [] app

bindSemToIO :: P.Member (P.Final IO) r => (p -> Sem r a) -> Sem r (p -> IO (Maybe a))
bindSemToIO m = P.withStrategicToFinal $ do
  istate <- P.getInitialStateS
  m' <- P.bindS m
  ins <- P.getInspectorS
  P.liftS $ pure (\x -> P.inspect ins <$> m' (istate $> x))
