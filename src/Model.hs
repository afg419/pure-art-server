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
import Data.Singletons.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

data GInsert = Get | Insert

newtype SCanvas2 (a :: Asset) (m :: Nat) (n :: Nat) = SCanvas2 Canvas2

canvas2Plane :: KnownNats m n => painting (m :: Nat) (n :: Nat) -> Plane2 m n
canvas2Plane _ = P2

newtype SCanvas2Id (a:: Asset) (m :: Nat) (n :: Nat) = SCanvas2Id Canvas2Id
instance Show (SCanvas2Id a m n) where
  show (SCanvas2Id s) = show s

data SomeCanvasId where
  CanvasIdExists :: SCanvas2Id a m n -> SomeCanvasId
instance Show SomeCanvasId where
  show (CanvasIdExists s) = show s

newtype SLocaleId (a :: Asset) (m :: Nat) (n :: Nat) = SLocaleId LocaleRecordId

toLocaleRecord :: forall a m n. SingI a => SCanvas2Id a m n -> UTCTime -> SLocale a m n -> LocaleRecord
toLocaleRecord (SCanvas2Id canvasId) now (SLocale {..}) = LocaleRecord canvasId rX rY lPath (tshow lAddress) now now
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

toLocaleRecordKey :: SCanvas2Id a m n -> SLocale a m n -> Key LocaleRecord
toLocaleRecordKey (SCanvas2Id canvasId) (SLocale {..}) = LocaleRecordKey canvasId rX rY
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

fromLocaleRecord :: KnownNats m n => SAsset a -> LocaleRecord -> (SLocale a m n)
fromLocaleRecord a LocaleRecord{..} = SLocale coordinate address localeRecordPath
  where
    coordinate = SCoordinate2 (fromIntegral localeRecordX) (fromIntegral localeRecordY)
    address = fromJust <<< mkAddress a $ localeRecordAddress
