module Effects where

import Import

-- The type class of a monad interpretable into Handler
class Monad s => Effect s where
  run :: s a -> Handler a

-- a type wrapper which allows us to pass the above run methods into functions
newtype Eff s = Eff { ex :: forall a. s a -> Handler a}

-- effectRunners will consist of a tuple of (Eff s). These will be read
-- and used to interpret various Effect monads into Handler
type Effectful effectRunners a = Reader effectRunners (Handler a)

runEffects :: effectRunners -> Effectful effectRunners a -> Handler a
runEffects e eff = runIdentity <<< flip runReaderT e $ eff
