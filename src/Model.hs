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

-- only made with mkSCanvas2 which ensures a m n match canvas2
newtype SCanvas2 (a :: Asset) (m :: Nat) (n :: Nat) = SCanvas2 Canvas2

mkSCanvas2 :: CTY a m n -> Canvas2 -> Maybe (SCanvas2 a m n)
mkSCanvas2 (CTY sa sxsy) c2 = if (fromSing sa, sx, sy) == canvasToCanvasTY c2
  then Just (SCanvas2 c2)
  else Nothing
  where
    (sx, sy) = dimensions sxsy

canvasToCanvasTY :: Canvas2 -> (Asset, Natural, Natural)
canvasToCanvasTY Canvas2{..} =
  ( canvas2Asset
  , fromIntegral canvas2XSize
  , fromIntegral canvas2YSize
  )


data SCanvas2Id (a:: Asset) (m :: Nat) (n :: Nat) = SCanvas2Id { cid :: Canvas2Id }

instance Show (SCanvas2Id a m n) where
  show (SCanvas2Id s) = show s

data SomeCanvasId where
  CanvasIdExists :: SCanvas2Id a m n -> SomeCanvasId

instance Show SomeCanvasId where
  show (CanvasIdExists s) = show s

newtype SLocaleId (a :: Asset) (m :: Nat) (n :: Nat) = SLocaleId LocaleRecordId

toLocaleRecord :: forall a m n. SCanvas2Id a m n -> UTCTime -> SLocale a m n -> LocaleRecord
toLocaleRecord (SCanvas2Id canvasId) now (SLocale {..}) = LocaleRecord canvasId rX rY lPath (tshow lAddress) now now
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

toLocaleRecordKey :: SCanvas2Id a m n -> SLocale a m n -> Key LocaleRecord
toLocaleRecordKey (SCanvas2Id canvasId) (SLocale {..}) = LocaleRecordKey canvasId rX rY
  where
    rX = fromIntegral $ x lCoordinate
    rY = fromIntegral $ y lCoordinate

fromLocaleRecord :: CTY a m n -> LocaleRecord -> Maybe (SLocale a m n)
fromLocaleRecord cty LocaleRecord{..} = do
  coordinate <- mkCoordinate (fromIntegral localeRecordX) (fromIntegral localeRecordY) (dim cty)
  pure $ SLocale coordinate address localeRecordPath
  where
    address = fromJust <<< mkAddress (sAsset cty) $ localeRecordAddress
