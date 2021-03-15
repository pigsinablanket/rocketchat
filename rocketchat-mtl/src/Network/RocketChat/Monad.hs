{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.RocketChat.Monad where

import Relude
import Network.RocketChat.Monad.UUID
import Network.RocketChat.Monad.Config
import Network.RocketChat.Monad.WebSocket
import Network.RocketChat.Monad.Logging

newtype RocketChatT m a = RocketChatT { unRocketChatT :: ReaderT () m a }
    deriving ( Monad
             , Applicative
             , Functor
             , MonadReader ()
             , MonadIO
             , MonadTrans
             )

deriving instance MonadUUID m => MonadUUID (RocketChatT m)
deriving instance MonadConfig m => MonadConfig (RocketChatT m)
deriving instance MonadWebSocket m => MonadWebSocket (RocketChatT m)
deriving instance MonadLogging m => MonadLogging (RocketChatT m)

runRocketChatT :: RocketChatT m a -> m a
runRocketChatT op = usingReaderT () (unRocketChatT op)
