{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Effects.Interpreters where

import Import hiding (fail)
import Effects.Common

newtype PsqlDB a = PsqlDB { runPsql :: SqlPersistT Handler a } deriving (Functor, Applicative, Monad, MonadIO)

instance Effect PsqlDB where
  run = Interpreter $ runDB <<< runPsql
