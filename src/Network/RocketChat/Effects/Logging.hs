module Network.RocketChat.Effects.Logging where

import           Polysemy
import           Relude

data LoggingE m a where
  Debug :: Text -> LoggingE m ()
  Warning :: Text -> LoggingE m ()
  Error :: Text -> LoggingE m ()

makeSem ''LoggingE

runLogging :: Members '[Embed IO] r => Sem (LoggingE : r) a -> Sem r a
runLogging = interpret $ \case
  Debug s -> embed $ print $ "[DEBUG] " <> s
  Warning s -> embed $ print $ "[WARNING] " <> s
  Error s -> embed $ print $ "[ERROR] " <> s
