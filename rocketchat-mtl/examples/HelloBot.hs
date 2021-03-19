module Main (main) where

import Network.RocketChat.Types as RC
import Network.RocketChat.Monad

main :: IO ()
main = runRocketChat handler "examples/config.ini"

handler :: MonadRocketChat m => Message -> m ()
handler  msg = do
  case message_type msg of
    _  -> pure ()
