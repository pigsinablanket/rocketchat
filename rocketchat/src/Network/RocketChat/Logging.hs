{-# LANGUAGE OverloadedStrings #-}

module Network.RocketChat.Logging where

import           Data.Time (getZonedTime)
import           Relude

-- | Logs a sent message to terminal
debug :: Show a => a -> IO ()
debug s = getZonedTime
            >>= \time -> putStrLn $ "[DEBUG] " ++ (show time) ++ " " ++ (show s)

-- | Logs a sent message to terminal
warning :: Show a => a -> IO ()
warning s = getZonedTime
            >>= \time -> putStrLn $ "[WARNING] " ++ (show time) ++ " " ++ (show s)

-- | Logs a sent message to terminal
err :: Show a => a -> IO ()
err s = getZonedTime
            >>= \time -> putStrLn $ "[ERROR] " ++ (show time) ++ " " ++ (show s)
