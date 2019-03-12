{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad             (forever)
import           Data.Text                 (Text)
import qualified Data.UUID.V4              as UUID (nextRandom)
import qualified Data.Text.IO              as T
import           Network.RocketChat        as RC
import qualified Network.WebSockets        as WS

main :: IO ()
main = do
  initialize bot "rocketchat.domain.com" 443

bot :: WS.ClientApp ()
bot conn = do
  connect conn defaultConnectRequest

  uuid <- UUID.nextRandom

  login conn $ defaultLoginRequest {
    RC.id = uuid
    , params = [ Credentials
                 (Username "hellobot")
                 (encode_pass "password")
               ]
    }

  _ <- forever $ do
      message <- WS.receiveData conn
      T.putStrLn message
      handle_message conn message

  WS.sendClose conn ("Disconnecting..." :: Text)

handle_message :: WS.Connection -> Text -> IO ()
handle_message conn msg = do
  print (message_type msg)
  case message_type msg of
    Just Ping -> RC.send_ping conn
    _         -> return ()
