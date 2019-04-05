{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.RocketChat.Types where

import Data.Aeson
import Data.Text (Text)
import Data.UUID (nil, UUID)
import GHC.Generics
import Network.Socket (HostName, PortNumber)
import Network.WebSockets (Connection)

data RC_Instance = RC_Instance Connection Config

type Handler = RC_Instance -> Message -> IO ()

-- |
-- Possible message responses from the webserver
data MessageResponse = Added
                     | Changed
                     | Connected
                     | Ping
                     | Ready
                     | Result
                     | Updated
                     deriving Show

type Message = Text

--data MethodParam a = MethodParam a
--data SubParam a    = SubParam a

-- |
-- Custom option to remove everything before _ from record fields
customOptions :: Options
customOptions = defaultOptions {
    fieldLabelModifier = tail . dropWhile (/= '_')
  }

data Config = Config {
    cf_host     :: HostName
  , cf_port     :: PortNumber
  , cf_username :: Text
  , cf_password :: Text
  } deriving (Eq, Show)

data ConnectRequest = ConnectRequest {
    cr_msg     :: Text
  , cr_version :: Text
  , cr_support :: [Text]
  } deriving (Generic, Show)
instance ToJSON ConnectRequest where
  toEncoding = genericToEncoding customOptions
instance FromJSON ConnectRequest

defaultConnectRequest :: ConnectRequest
defaultConnectRequest = ConnectRequest {
    cr_msg     = "connect"
  , cr_version = "1"
  , cr_support = ["1", "pre2", "pre1"]
  }

data Username = Username {
    _username :: Text
  } deriving (Generic, Show)
instance ToJSON Username where
  toEncoding = genericToEncoding customOptions
instance FromJSON Username

data Password = Password {
    _digest    :: Text
  , _algorithm :: Text
  } deriving (Generic, Show)
instance ToJSON Password where
  toEncoding = genericToEncoding customOptions
instance FromJSON Password

data Credentials = Credentials {
    _user     :: Username
  , _password :: Password
  } deriving (Generic, Show)
instance ToJSON Credentials where
  toEncoding = genericToEncoding customOptions
instance FromJSON Credentials

data MethodRequest = MethodRequest {
    mr_msg    :: Text
  , mr_method :: Text
  , mr_id     :: UUID
  , mr_params :: [Credentials]
  } deriving (Generic, Show)
instance ToJSON MethodRequest where
  toEncoding = genericToEncoding customOptions
instance FromJSON MethodRequest

defaultLoginRequest :: MethodRequest
defaultLoginRequest = MethodRequest {
    mr_msg    = "method"
  , mr_method = "login"
  , mr_id     = nil
  , mr_params = []
  }

data PingResponse = PingResponse {
    pr_msg :: Text
  } deriving (Generic, Show)
instance ToJSON PingResponse where
  toEncoding = genericToEncoding customOptions
instance FromJSON PingResponse

pong :: PingResponse
pong = PingResponse { pr_msg="pong" }

data SubscribeRequest = SubscribeRequest {
    sr_msg    :: Text
  , sr_id     :: UUID
  , sr_name   :: Text
  , sr_params :: (Text, Bool)
  } deriving (Generic, Show)
instance ToJSON SubscribeRequest where
  toEncoding = genericToEncoding customOptions
instance FromJSON SubscribeRequest

defaultRoomRequest :: SubscribeRequest
defaultRoomRequest = SubscribeRequest {
    sr_msg    = "sub"
  , sr_id     = nil
  , sr_name   = "stream-room-messages"
  , sr_params = ("", False)
  }

defaultSelfRequest :: SubscribeRequest
defaultSelfRequest = SubscribeRequest {
    sr_msg    = "sub"
  , sr_id     = nil
  , sr_name   = "stream-notify-user"
  , sr_params = ("", False)
  }
