module Main (main) where

import Network.RocketChat.Types as RC
import Network.RocketChat.Polysemy as RC
import Polysemy
import Polysemy.Async

main :: IO ()
main = runRocketChat handler "examples/config.ini"

handler :: Members RocketChatE r => Message -> Sem r ()
handler msg = do
    case message_type msg of
      _  -> RC.defaultHandler msg
