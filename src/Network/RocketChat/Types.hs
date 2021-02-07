{-# LANGUAGE DeriveGeneric #-}

module Network.RocketChat.Types where

import Data.Aeson
import Data.String.Utils (replace)
import Data.UUID (UUID, nil)
import GHC.Generics
import Network.Socket (HostName, PortNumber)
import Network.WebSockets (Connection)
import Relude hiding (tail)
import Relude.Unsafe (tail)

-- |
-- The instance for a rocketchat connection
data RC_Instance = RC_Instance Connection Config

data Config = Config {
    cf_host     :: HostName
  , cf_port     :: PortNumber
  , cf_username :: Text
  , cf_password :: Text
  } deriving (Eq, Show)

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

-- |
-- Custom options to remove everything before _ from record fields
customOptions :: Options
customOptions = defaultOptions {
    sumEncoding        = UntaggedValue
  , fieldLabelModifier = tail
                         . dropWhile (/= '_')
                         . replace "DOLLAR" "$"
  }

defaultConnectRequest :: Request
defaultConnectRequest = ConnectRequest {
    cr_msg     = "connect"
  , cr_version = "1"
  , cr_support = ["1"]
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

data MethodParams =
  Credentials {
    _user     :: Username
  , _password :: Password
  }
  | Date {
      _DOLLARdate :: Int
  }
  deriving (Generic, Show)
instance ToJSON MethodParams where
  toEncoding = genericToEncoding customOptions
instance FromJSON MethodParams

data PingResponse = PingResponse {
    pr_msg :: Text
  } deriving (Generic, Show)
instance ToJSON PingResponse where
  toEncoding = genericToEncoding customOptions
instance FromJSON PingResponse

pong :: PingResponse
pong = PingResponse { pr_msg="pong" }

-- | The base type that represents a method call
data Request = MethodRequest {
    mr_msg    :: Text
  , mr_method :: Text
  , mr_id     :: UUID
  , mr_params :: [MethodParams]
  } |
  SubscribeRequest {
    sr_msg    :: Text
  , sr_id     :: UUID
  , sr_name   :: Text
  , sr_params :: (Text, Bool)
  } |
  ConnectRequest {
    cr_msg     :: Text
  , cr_version :: Text
  , cr_support :: [Text]
  } deriving (Generic, Show)
instance ToJSON Request where
  toEncoding = genericToEncoding customOptions
instance FromJSON Request

-- Method Call Requests

methodRequest :: Request
methodRequest = MethodRequest {
    mr_msg    = "method"
  , mr_method = ""
  , mr_id     = nil
  , mr_params = []
  }

loginRequest :: Request
loginRequest = methodRequest { mr_method = "login" }

getRoomsRequest :: Request
getRoomsRequest = methodRequest { mr_method = "rooms/get"
                                , mr_params = [(Date { _DOLLARdate = 0 })] }

joinRoomRequest :: Request
joinRoomRequest = methodRequest { mr_method = "joinRoom" }

publicSettingsRequest :: Request
publicSettingsRequest = methodRequest { mr_method = "public-settings/get" }

-- Subscribe Requests

defaultRoomRequest :: Request
defaultRoomRequest = SubscribeRequest {
    sr_msg    = "sub"
  , sr_id     = nil
  , sr_name   = "stream-room-messages"
  , sr_params = ("", False)
  }

defaultSelfRequest :: Request
defaultSelfRequest = SubscribeRequest {
    sr_msg    = "sub"
  , sr_id     = nil
  , sr_name   = "stream-notify-user"
  , sr_params = ("", False)
  }
