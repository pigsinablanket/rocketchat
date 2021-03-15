{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.RocketChat.Monad.Logging where

import Relude

class Monad m => MonadLogging m where
    debug :: Text -> m ()
    warning :: Text -> m ()
    err :: Text -> m ()

instance MonadLogging IO where
    debug s = liftIO $ print $ "[DEBUG] " <> s
    warning s = liftIO $ print $ "[WARNING] " <> s
    err s = liftIO $ print $ "[ERROR] " <> s

instance MonadLogging m => MonadLogging (ReaderT r m) where
    debug s = lift (debug s)
    warning s = lift (warning s)
    err s = lift (err s)
