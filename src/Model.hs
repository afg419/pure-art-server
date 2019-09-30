{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}


module Model where

import ClassyPrelude.Yesod
import PointGen
import Import
import           Database.Persist.Sql

--------------------------------------------------------------------------------
-- Models
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  PaintingRecord
    asset Asset
    xSize Word64
    ySize Word64
    xPub XPub -- extract to account
    nextPathIndex Word64 -- extract to generator for this painting
    fullyApproximated Bool
    createdAt UTCTime
    updatedAt UTCTime
    deriving Show

  LocaleRecord
    paintingRecordId PaintingRecordId
    path DerivationPath
    address Text
    x Word64
    y Word64
    createdAt UTCTime
    PaintingCoordinate paintingRecordId x y
    deriving Show

  VertexRecord
    paintingRecordId PaintingRecordId
    x Word64
    y Word64
    closestLocale LocaleRecordId Maybe
    createdAt UTCTime
    Primary paintingRecordId x y
    deriving Show

  EdgeRecord
    paintingRecordId PaintingRecordId
    fromX Word64
    fromY Word64
    toX Word64
    toY Word64
    createdAt UTCTime
    Foreign VertexRecord from paintingRecordId fromX fromY
    Foreign VertexRecord to paintingRecordId toX toY
    deriving Show
  |]


mkSafePainting :: forall a m n. SCTY a m n -> PaintingRecord -> Maybe (SafeCTY a m n PaintingRecord)
mkSafePainting = mkSafeCTY Equals

getVertexRecordId :: VertexRecord -> VertexRecordId
getVertexRecordId VertexRecord{..} =
  VertexRecordKey
    vertexRecordPaintingRecordId
    vertexRecordX
    vertexRecordY

instance TwoDimensional LocaleRecord where
  getX = fromIntegral <<< localeRecordX
  getY = fromIntegral <<< localeRecordY

instance TwoDimensional VertexRecord where
  getX = fromIntegral <<< vertexRecordX
  getY = fromIntegral <<< vertexRecordY

instance TwoDimensional VertexRecordId where
  getX (VertexRecordKey _ x _) = fromIntegral x
  getY (VertexRecordKey _ _ y) = fromIntegral y

instance TwoDimensional PaintingRecord where
  getX = fromIntegral <<< paintingRecordXSize
  getY = fromIntegral <<< paintingRecordYSize

instance CTY PaintingRecord where
  getAsset = paintingRecordAsset

instance TwoDimensional (Entity PaintingRecord) where
  getX = getX <<< entityVal
  getY = getY <<< entityVal
instance CTY (Entity PaintingRecord) where
  getAsset = getAsset <<< entityVal

getVertexRecord :: VertexRecordApproximation a m n -> Safe2D m n VertexRecord
getVertexRecord (PerfectRecord v _) = v
getVertexRecord (ApproximateRecord v _) = v
getVertexRecord (NoRecord v ) = v

data VertexRecordApproximation (a :: Asset) (m :: Nat) (n :: Nat) where
  PerfectRecord :: Safe2D m n VertexRecord -> SafeCTY a m n LocaleRecord -> VertexRecordApproximation a m n
  ApproximateRecord :: Safe2D m n VertexRecord -> SafeCTY a m n LocaleRecord -> VertexRecordApproximation a m n
  NoRecord :: Safe2D m n VertexRecord -> VertexRecordApproximation a m n
instance Show (VertexRecordApproximation a m n) where
  show = show <<< fromSafe2D <<< getVertexRecord


data ApproximationQuality = Perfect | Approx Natural | None deriving Eq
getApproximationQuality :: VertexRecordApproximation a m n -> ApproximationQuality
getApproximationQuality (PerfectRecord _ _) = Perfect
getApproximationQuality (ApproximateRecord v l) = Approx <<$ l1Dist v l
getApproximationQuality (NoRecord _) = None


instance TwoDimensional (VertexRecordApproximation a m n) where
  getX = getX <<< getVertexRecord
  getY = getY <<< getVertexRecord
