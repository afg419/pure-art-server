{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Effects.Interpreters where

import Foundation
import Import
import Effects.Common
import Database.Persist.Sql

newtype PsqlDB a = PsqlDB { runPsql :: SqlPersistT IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance Effect PsqlDB where
  run = Interpreter $ \(PsqlDB persisty) -> do
    connectionPool <- fmap appConnPool ask
    liftIO $ runSqlPool persisty connectionPool
