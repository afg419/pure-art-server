{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric               #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import PointGen
import Import

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

data GInsert = Get | Insert

newtype SCanvas2 (m :: Nat) (n :: Nat) (a :: Asset) = SCanvas2 Canvas2
canvas2Plane :: KnownNats m n => painting (m :: Nat) (n :: Nat) (a :: Asset) -> Plane2 m n
canvas2Plane _ = P2

newtype SCanvas2Id (m :: Nat) (n :: Nat) (a :: Asset) = SCanvas2Id Canvas2Id
instance Show (SCanvas2Id m n a) where
  show (SCanvas2Id s) = show s

data SomeCanvasId where
  CanvasIdExists :: SCanvas2Id m n a -> SomeCanvasId
instance Show SomeCanvasId where
  show (CanvasIdExists s) = show s

newtype SXPubId = SXPubId XPubRecordId deriving (Show, Eq, Generic)
instance FromJSON SXPubId

newtype SLocaleId (m :: Nat) (n :: Nat) (a :: Asset) = SLocaleId LocaleRecordId
