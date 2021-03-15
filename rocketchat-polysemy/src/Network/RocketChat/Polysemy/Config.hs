module Network.RocketChat.Polysemy.Config where

import Network.RocketChat.Config (readAndParseConfig)
import Network.RocketChat.Types
import Polysemy
import Relude

data ConfigE m a where
  GetConfig :: ConfigE m Config

makeSem ''ConfigE

runConfig :: Members '[Embed IO] r => FilePath -> Sem (ConfigE : r) a -> Sem r a
runConfig filePath = do
    interpret $ \case
        GetConfig -> embed $ readAndParseConfig filePath
