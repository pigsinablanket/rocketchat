module Main (main) where

import Network.RocketChat as RC
import Polysemy

main :: IO ()
main = run handler "examples/config.ini"

handler :: Members [RocketChatE,LoggingE,ConfigE,UUID] r
        => RC_Instance -> Message -> Sem r () -- Handler
handler rc_instance@(RC_Instance _conn _config) msg = do
  _uuid <- RC.gen_uuid
  case message_type msg of
    _  -> RC.default_handler rc_instance msg
