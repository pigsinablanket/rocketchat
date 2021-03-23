module Network.RocketChat.Polysemy.UUID where

import qualified Data.UUID as UUID (UUID)
import qualified Data.UUID.V4 as UUID (nextRandom)
import           Polysemy
import           Relude

data UUIDE m a where
  -- | Generates a random UUID
  GenUUID :: UUIDE m UUID.UUID

makeSem ''UUIDE

runUUID :: Members '[Embed IO] r => Sem (UUIDE : r) a -> Sem r a
runUUID = interpret $ \case
  GenUUID -> embed $ UUID.nextRandom
