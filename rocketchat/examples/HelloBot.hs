module Main (main) where

import Network.RocketChat as RC

main :: IO ()
main = runRocketChat handler "examples/config.ini"

handler :: RC_Instance -> Message -> IO ()
handler rc_instance@(RC_Instance _conn _config) msg = do
  case message_type msg of
    _  -> RC.defaultHandler rc_instance msg
