module Network.RocketChat.Monad.UUID where

import qualified Data.UUID as UUID (UUID)
import qualified Data.UUID.V4 as UUID (nextRandom)
import           Relude

class Monad m => MonadUUID m where
    genUUID :: m UUID.UUID

instance MonadUUID IO where
    genUUID = UUID.nextRandom

instance MonadUUID m => MonadUUID (ReaderT r m) where
    genUUID = lift genUUID
