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

newtype SCanvas2 (m :: Nat) (n :: Nat) = SCanvas2 Canvas2
canvas2Plane :: KnownNats m n => painting (m :: Nat) (n :: Nat) -> Plane2 m n
canvas2Plane _ = P2

newtype SCanvas2Id (m :: Nat) (n :: Nat) = SCanvas2Id Canvas2Id
instance Show (SCanvas2Id m n) where
  show (SCanvas2Id s) = show s

data SomeCanvasId where
  CanvasIdExists :: SCanvas2Id m n -> SomeCanvasId
instance Show SomeCanvasId where
  show (CanvasIdExists s) = show s

newtype SLocaleId (a :: Asset) (m :: Nat) (n :: Nat) = SLocaleId LocaleRecordId

toLocaleRecord :: forall a m n. SingI a => SCanvas2Id m n -> UTCTime -> Locale a m n -> LocaleRecord
toLocaleRecord (SCanvas2Id canvasId) now (Locale {..}) = LocaleRecord canvasId rX rY lPath (tshow lAddress) (fromSing asset) now now
  where
    asset = sing :: SAsset a
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

toLocaleRecordKey :: SCanvas2Id m n -> Locale a m n -> Key LocaleRecord
toLocaleRecordKey (SCanvas2Id canvasId) (Locale {..}) = LocaleRecordKey canvasId rX rY
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

fromLocaleRecord :: KnownNats m n => SAsset a -> SCanvas2Id m n -> LocaleRecord -> Maybe (Locale a m n)
fromLocaleRecord sAsset _ LocaleRecord{..} = if hasCorrectAsset
  then Just $ Locale coordinate address localeRecordPath
  else Nothing
  where
    hasCorrectAsset = localeRecordAsset == fromSing sAsset
    coordinate = Coordinate2 (fromIntegral localeRecordX) (fromIntegral localeRecordY) P2
    address = fromJust <<< mkAddress sAsset $ localeRecordAddress
