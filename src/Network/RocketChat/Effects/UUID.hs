module Network.RocketChat.Effects.UUID where

import qualified Data.UUID as UUID (UUID)
import qualified Data.UUID.V4 as UUID (nextRandom)
import           Polysemy
import           Relude

data UUID m a where
  -- | Generates a random UUID
  GenUUID :: UUID m UUID.UUID

makeSem ''UUID

runUUID :: Members '[Embed IO] r => Sem (UUID : r) a -> Sem r a
runUUID = interpret $ \case
  GenUUID -> embed $ UUID.nextRandom
