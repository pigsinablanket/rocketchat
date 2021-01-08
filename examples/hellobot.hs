{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.RocketChat as RC

main :: IO ()
main = run handler "examples/config.ini"

handler :: Handler
handler rc_instance@(RC_Instance conn config) msg = do
  uuid <- RC.gen_uuid
  case message_type msg of
    _  -> RC.default_handler rc_instance msg
