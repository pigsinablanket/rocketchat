{-# LANGUAGE OverloadedStrings #-}

module Network.RocketChat.Logging where

import           Data.Time (getZonedTime)
import           Relude

-- | Logs a sent message to terminal
log_msg_send :: Show a => a -> IO ()
log_msg_send s = getZonedTime
            >>= \time -> putStrLn $ "SENT:     " ++ (show time) ++ "\t" ++ (show s)

-- | Logs a recieved message to terminal
log_msg_recv :: Show a => a -> IO ()
log_msg_recv s = getZonedTime
            >>= \time -> putStrLn $ "RECEIVED: " ++ (show time) ++ "\t" ++ (show s)
