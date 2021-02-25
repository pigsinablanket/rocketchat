module Network.RocketChat.Effects.Config where

import Network.RocketChat.Config (readAndParseConfig)
import Network.RocketChat.Types
import Polysemy
import Relude

data ConfigE m a where
  GetConfig :: FilePath -> ConfigE m Config

makeSem ''ConfigE

runConfig :: Members '[Embed IO] r => Sem (ConfigE : r) a -> Sem r a
runConfig = interpret $ \case
  GetConfig filepath -> embed $ readAndParseConfig filepath
