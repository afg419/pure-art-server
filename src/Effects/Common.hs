module Effects.Common where

import Import

-- a wrapper for an interpreter from s to Handler
newtype Interpreter s = Interpreter { interpret :: forall a t. MonadTrans t => s a -> t Handler a }

-- The type class of a monad interpretable into Handler
class Monad s => Effect s where
  run :: Interpreter s

-- interpreters will consist of a tuple of Interpreters. These will be read
-- and used to interpret various Effect monads into Handler
type Effectful interpreters = ReaderT interpreters Handler

runEffects :: interpreters -> Effectful interpreters a -> Handler a
runEffects e eff = flip runReaderT e $ eff
