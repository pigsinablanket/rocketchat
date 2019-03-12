{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Network.RocketChat.Types where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.UUID (nil, UUID)

-- |
-- Possible message responses from the webserver
data Message = Added
             | Changed
             | Connected
             | Ping
             | Ready
             | Result
             | Updated
  deriving Show

data ConnectRequest = ConnectRequest {
    msg :: Text
  , version :: Text
  , support :: [Text]
  } deriving (Generic, Show)
instance ToJSON ConnectRequest where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON ConnectRequest

defaultConnectRequest :: ConnectRequest
defaultConnectRequest = ConnectRequest { msg="connect"
                                       , version="1"
                                       , support=["1", "pre2", "pre1"] }

data Username = Username {
  username :: Text
  } deriving (Generic, Show)
instance ToJSON Username where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Username

data Password = Password {
    digest :: Text
  , algorithm :: Text
  } deriving (Generic, Show)
instance ToJSON Password where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Password

data Credentials = Credentials {
    user :: Username
  , password :: Password
  } deriving (Generic, Show)
instance ToJSON Credentials where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Credentials

data LoginRequest = LoginRequest {
    msg :: Text
  , method :: Text
  , id :: UUID
  , params :: [Credentials]
  } deriving (Generic, Show)
instance ToJSON LoginRequest where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON LoginRequest

defaultLoginRequest :: LoginRequest
defaultLoginRequest = LoginRequest { msg="method"
                                   , method="login"
                                   , id=nil
                                   , params=[] }

data PingResponse = PingResponse {
    msg :: Text
  } deriving (Generic, Show)
instance ToJSON PingResponse where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON PingResponse

pong :: PingResponse
pong = PingResponse { msg="pong" }



type Hostname = Text
type Port     = Int
