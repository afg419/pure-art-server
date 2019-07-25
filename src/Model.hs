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

newtype SLocaleId (m :: Nat) (n :: Nat) (a :: Asset) = SLocaleId LocaleRecordId

toLocaleRecord :: SCanvas2Id m n a -> UTCTime -> Locale m n a -> LocaleRecord
toLocaleRecord (SCanvas2Id canvasId) now (Locale {..}) = LocaleRecord canvasId rX rY lPath (tshow lAddress) now now
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

toLocaleRecordKey :: SCanvas2Id m n a -> Locale m n a -> Key LocaleRecord
toLocaleRecordKey (SCanvas2Id canvasId) (Locale {..}) = LocaleRecordKey canvasId rX rY
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate
-- data Locale (m :: Nat) (n :: Nat) (a :: Asset) = Locale { lCoordinate :: Coordinate2 m n, lAddress :: Address a, lPath :: DerivationPath } deriving Show
fromLocaleRecord :: KnownNats m n => SAsset a -> SCanvas2Id m n a -> LocaleRecord -> Locale m n a
fromLocaleRecord sAsset _ LocaleRecord{..} = Locale coordinate address localeRecordPath
  where
    coordinate = Coordinate2 (fromIntegral localeRecordX) (fromIntegral localeRecordY) P2
    address = fromJust <<< mkAddress sAsset $ localeRecordAddress

-- data Locale (m :: Nat) (n :: Nat) (a :: Asset) = Locale { lCoordinate :: Coordinate2 m n, lAddress :: Address a, lPath :: DerivationPath } deriving Show
