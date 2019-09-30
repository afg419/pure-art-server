{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Effects.Common where

import Foundation
import Import

data Interpreter s where
  Interpreter :: (forall a m. (MonadReader App m, MonadIO m) => s a -> m a) -> Interpreter s

interpret :: Interpreter s -> (forall a m. (MonadIO m) => s a -> ReaderT App m a)
interpret (Interpreter nat) = nat

-- The type class of a monad interpretable into (ReaderT App m a)
class Monad s => Effect s where
  run :: Interpreter s

liftEffectful :: forall s a m. (Effect s, MonadIO m) => App -> s a -> m a
liftEffectful app = flip runReaderT app <<< interpret run
