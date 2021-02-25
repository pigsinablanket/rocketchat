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
send_request :: WS.Connection -> Request -> IO ()
send_request conn request = do
  log_msg_send $ A.encode request
  WS.sendTextData conn $ A.encode request

-- | Close the connection to the webserver
close_connection :: WS.Connection -> IO ()
close_connection conn = WS.sendClose conn ("Disconnecting..." :: Text)

-- | Sends a connection request to the websocket
connect :: WS.Connection -> Request -> IO ()
connect conn connectRequest = do
  log_msg_send $ A.encode connectRequest
  WS.sendTextData conn $ A.encode connectRequest

-- | Sends a login request to the websocket
login :: WS.Connection -> Request -> IO ()
login conn lr = do
  log_msg_send $ A.encode lr
  WS.sendTextData conn $ A.encode lr

-- | Sends a ping to the websocket
send_ping :: WS.Connection -> IO ()
send_ping conn = do
  log_msg_send $ A.encode pong
  WS.sendTextData conn $ A.encode pong

-- | Get a list of currently joined rooms
get_rooms :: WS.Connection -> UUID -> IO () -- (Maybe Text)
get_rooms conn uuid = do
  log_msg_send $ A.encode $ getRoomsRequest { mr_id = uuid }
  WS.sendTextData conn $ A.encode $ getRoomsRequest { mr_id = uuid }
  _response <- listen_for_uuid conn uuid
  return () -- response

-- | Get current public server settings
get_public_settings :: WS.Connection -> UUID -> IO ()
get_public_settings conn uuid = do
  log_msg_send $ A.encode publicSettingsRequest { mr_id = uuid }
  WS.sendTextData conn $ A.encode publicSettingsRequest { mr_id = uuid }

listen_for_uuid :: WS.Connection -> UUID -> IO (Maybe Text)
listen_for_uuid conn _uuid = do
  msg <- WS.receiveData conn
  case parse_for_uuid msg of
    Just _  -> return $ Just msg
    Nothing -> return Nothing

parse_for_uuid :: Message -> Maybe UUID
parse_for_uuid msg = case (A.decodeStrict (encodeUtf8 msg)) of
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
