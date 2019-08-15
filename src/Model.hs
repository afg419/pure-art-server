{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import PointGen
import Import
import Data.Maybe

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

data GInsert = Get | Insert

newtype SCanvas2 (a :: Asset) (m :: Nat) (n :: Nat) = SCanvas2 Canvas2
canvas2Plane :: KnownNats m n => painting (m :: Nat) (n :: Nat) -> Plane2 m n
canvas2Plane _ = P2

newtype SCanvas2Id (a :: Asset) (m :: Nat) (n :: Nat) = SCanvas2Id Canvas2Id
instance Show (SCanvas2Id a m n) where
  show (SCanvas2Id s) = show s

data SomeCanvasId where
  CanvasIdExists :: SCanvas2Id a m n -> SomeCanvasId
instance Show SomeCanvasId where
  show (CanvasIdExists s) = show s

newtype SLocaleId (a :: Asset) (m :: Nat) (n :: Nat) = SLocaleId LocaleRecordId

toLocaleRecord :: SCanvas2Id a m n -> UTCTime -> Locale a m n -> LocaleRecord
toLocaleRecord (SCanvas2Id canvasId) now (Locale {..}) = LocaleRecord canvasId rX rY lPath (tshow lAddress) now now
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

toLocaleRecordKey :: SCanvas2Id a m n -> Locale a m n -> Key LocaleRecord
toLocaleRecordKey (SCanvas2Id canvasId) (Locale {..}) = LocaleRecordKey canvasId rX rY
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate
-- data Locale (m :: Nat) (n :: Nat) (a :: Asset) = Locale { lCoordinate :: Coordinate2 m n, lAddress :: Address a, lPath :: DerivationPath } deriving Show
fromLocaleRecord :: KnownNats m n => SAsset a -> SCanvas2Id a m n -> LocaleRecord -> Locale a m n
fromLocaleRecord sAsset _ LocaleRecord{..} = Locale coordinate address localeRecordPath
  where
    coordinate = Coordinate2 (fromIntegral localeRecordX) (fromIntegral localeRecordY) P2
    address = fromJust <<< mkAddress sAsset $ localeRecordAddress

-- data Locale (m :: Nat) (n :: Nat) (a :: Asset) = Locale { lCoordinate :: Coordinate2 m n, lAddress :: Address a, lPath :: DerivationPath } deriving Show
