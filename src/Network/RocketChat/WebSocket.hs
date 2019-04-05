{-# LANGUAGE OverloadedStrings #-}

module Network.RocketChat.WebSocket where

import qualified Data.Aeson         as A (encode)
import           Data.Text          as T (Text)
import qualified Network.WebSockets as WS

import           Network.RocketChat.Types

-- | Close the connection to the webserver
close_connection :: WS.Connection -> IO ()
close_connection conn = WS.sendClose conn ("Disconnecting..." :: Text)

-- | Sends a connection request to the websocket
connect :: WS.Connection -> ConnectRequest -> IO ()
connect conn connectRequest = do
  WS.sendTextData conn $ A.encode connectRequest

-- | Sends a login request to the websocket
login :: WS.Connection -> MethodRequest -> IO ()
login conn loginRequest = do
  WS.sendTextData conn $ A.encode loginRequest

-- | Sends a ping to the websocket
send_ping :: WS.Connection -> IO ()
send_ping conn = WS.sendTextData conn $ A.encode pong
