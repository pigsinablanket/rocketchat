-- module Network.RocketChat.Effects.RocketChat where

-- import Network.RocketChat.Types
-- import Network.RocketChat.WebSocket as RC
-- import Data.UUID (UUID)
-- import           Polysemy
-- import           Relude
-- import Network.WebSockets (Connection)
-- import qualified Network.WebSockets        as WS

-- data RocketChatE m a where

-- makeSem ''RocketChatE

-- runRocketChat :: Members '[Embed IO] r => Sem (RocketChatE : r) a -> Sem r a
-- runRocketChat = interpret $ \case
