{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.RocketChat.Monad.Config where

import Network.RocketChat.Config (readAndParseConfig)
import Network.RocketChat.Types
import Relude

class Monad m => MonadConfig m where
    getConfig :: FilePath -> m Config

instance MonadConfig IO where
    getConfig filePath = liftIO $ readAndParseConfig filePath

instance MonadConfig m => MonadConfig (ReaderT r m) where
    getConfig f = lift (getConfig f)
