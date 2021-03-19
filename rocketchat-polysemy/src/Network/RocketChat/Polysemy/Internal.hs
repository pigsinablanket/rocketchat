module Network.RocketChat.Polysemy.Internal
    ( bindSemToIO
    , embed_
    ) where

import           Polysemy
import qualified Polysemy.Final as P
import           Relude

-- | un-embeds a sem monad to the IO monad
bindSemToIO :: Member (P.Final IO) r => (p -> Sem r a) -> Sem r (p -> IO (Maybe a))
bindSemToIO m = P.withStrategicToFinal $ do
    istate <- P.getInitialStateS
    m' <- P.bindS m
    ins <- P.getInspectorS
    P.liftS $ pure (\x -> inspect ins <$> m' (istate $> x))

embed_ :: Member (Embed m) r => m a -> Sem r ()
embed_ m = embed m >> pure ()
