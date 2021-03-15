module Network.RocketChat.WebSocket where

import qualified Data.Aeson          as A
import qualified Data.List           as L (lookup)
import qualified Data.HashMap.Strict as HM (toList)
import qualified Data.UUID           as UUID
import           Data.UUID (UUID)
import qualified Network.WebSockets  as WS
import           Relude
import           Network.Connection
import           Network.Socket (HostName, PortNumber)
import qualified Network.WebSockets.Stream as WS
import           Network.RocketChat.Logging
import           Network.RocketChat.Types

startWebSocketConnection :: HostName -> PortNumber -> (WS.Connection -> IO a) -> IO a
startWebSocketConnection hostname port app = do
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
                          (maybe (return ()) (connectionPut con . toStrict))
  WS.runClientWithStream stream hostname "/websocket" WS.defaultConnectionOptions [] app

-- | Send a request and return the result
sendRequest :: WS.Connection -> Request -> IO ()
sendRequest conn request = do
  debug $ A.encode request
  WS.sendTextData conn $ A.encode request

-- | Close the connection to the webserver
closeConnection :: WS.Connection -> IO ()
closeConnection conn = WS.sendClose conn ("Disconnecting..." :: Text)

-- | Sends a connection request to the websocket
connect :: WS.Connection -> Request -> IO ()
connect conn connectRequest = do
  debug $ A.encode connectRequest
  WS.sendTextData conn $ A.encode connectRequest

-- | Sends a login request to the websocket
login :: WS.Connection -> Request -> IO ()
login conn lr = do
  debug $ A.encode lr
  WS.sendTextData conn $ A.encode lr

-- | Sends a ping to the websocket
sendPing :: WS.Connection -> IO ()
sendPing conn = do
  debug $ A.encode pong
  WS.sendTextData conn $ A.encode pong

-- | Get a list of currently joined rooms
getRooms :: WS.Connection -> UUID -> IO () -- (Maybe Text)
getRooms conn uuid = do
  debug $ A.encode $ getRoomsRequest { mr_id = uuid }
  WS.sendTextData conn $ A.encode $ getRoomsRequest { mr_id = uuid }
  _response <- listenForUUID conn uuid
  return () -- response

-- | Get current public server settings
getPublicSettings :: WS.Connection -> UUID -> IO ()
getPublicSettings conn uuid = do
  debug $ A.encode publicSettingsRequest { mr_id = uuid }
  WS.sendTextData conn $ A.encode publicSettingsRequest { mr_id = uuid }

listenForUUID :: WS.Connection -> UUID -> IO (Maybe Text)
listenForUUID conn _uuid = do
  msg <- WS.receiveData conn
  case parseForUUID msg of
    Just _  -> return $ Just msg
    Nothing -> return Nothing

parseForUUID :: Message -> Maybe UUID
parseForUUID msg = case (A.decodeStrict (encodeUtf8 msg)) of
                       Just x  -> uuid_field x
                       Nothing -> Nothing
  where
    uuid_field (A.Object value) = case L.lookup "id" (HM.toList value) of
                                    Just x  -> uuid_field x
                                    Nothing -> Nothing
    uuid_field (A.String uuid)  = UUID.fromText uuid
    uuid_field _                = Nothing

{-
{
    "msg": "method",
    "method": "rooms/get",
    "id": "42",
    "params": [ { "$date": 1480377601 } ]
}

-}
