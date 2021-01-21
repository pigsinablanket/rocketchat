module Network.RocketChat.Effects.Logging where

import           Polysemy
import           Relude

data Logging m a where
  Debug :: Text -> Logging m ()
  Warning :: Text -> Logging m ()
  Error :: Text -> Logging m ()

makeSem ''Logging

runLoggingIO :: Members '[Embed IO] r => Sem (Logging : r) a -> Sem r a
runLoggingIO = interpret $ \case
  Debug s -> embed $ print $ "[DEBUG] " <> s
  Warning s -> embed $ print $ "[WARNING] " <> s
  Error s -> embed $ print $ "[ERROR] " <> s
