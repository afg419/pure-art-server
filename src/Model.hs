{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import PointGen
import GHC.TypeNats

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

-- data SPaintingRecord (a :: Asset) (m :: Nat) (n :: Nat) = SPaintingRecord PaintingRecord
-- data SPaintingRecordId (a :: Asset) (m :: Nat) (n :: Nat) = SPaintingRecordId PaintingRecordId
-- data SPaintingRecord (a :: Asset) (m :: Nat) (n :: Nat) = SPaintingRecord PaintingRecord

data Safe (s :: *) (a :: Asset) (m :: Nat) (n :: Nat) = Safe s
fromSafe :: Safe s a m n -> s
fromSafe (Safe s) = s
