{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent        (forkIO)
import           Control.Monad             (forever, unless)
import           Data.Maybe                (maybe)
import           Data.Text                 (Text)
import           Network.Connection
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Network.RocketChat
import qualified Network.WebSockets        as WS
import qualified Network.WebSockets.Stream as WS

main :: IO ()
main = do
  initialize bot "rocket.cat.pdx.edu" 443

bot :: WS.ClientApp ()
bot conn = do
  connect conn defaultConnectRequest

  _ <- forkIO $ forever $ do
    msg <- WS.receiveData conn
    T.putStrLn msg

  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop

  loop
  WS.sendClose conn ("Bye!" :: Text)
