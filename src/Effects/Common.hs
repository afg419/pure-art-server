module Effects.Common where

import Import

-- a wrapper for an interpreter from s to Handler
newtype Interpreter s = Interpreter { interpret :: forall a. s a -> Handler a }

-- The type class of a monad interpretable into Handler
class Monad s => Effect s where
  run :: Interpreter s

-- interpreters will consist of a tuple of Interpreters. These will be read
-- and used to interpret various Effect monads into Handler
type Effectful interpreters a = Reader interpreters (Handler a)

runEffects :: interpreters -> Effectful interpreters a -> Handler a
runEffects e eff = runIdentity <<< flip runReaderT e $ eff
