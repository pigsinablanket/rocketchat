module Main (main) where

import Network.RocketChat as RC
import Polysemy

main :: IO ()
main = runRocketChat handler "examples/config.ini"

handler :: Members [WebSocketE,LoggingE,ConfigE,UUID] r
        => RC_Instance -> Message -> Sem r ()
handler rc_instance@(RC_Instance _conn _config) msg = do
  case message_type msg of
    _  -> RC.default_handler rc_instance msg
