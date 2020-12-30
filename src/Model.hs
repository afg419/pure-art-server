{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}

module Model where

import ClassyPrelude.Yesod hiding (Proxy)
import PointGen
import Import
import           Database.Persist.Sql
import GHC.TypeNats
import Data.Singletons.TH
import Data.Maybe (fromJust)


--------------------------------------------------------------------------------
-- Models
--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  -- Generator
  --   asset Asset
  --   xSize Word64
  --   ySize Word64
  --   xPub XPub -- extract to account
  --   nextPathIndex Word64
  --   fullyApproximated Bool
  --   createdAt UTCTime
  --   updatedAt UTCTime
  --   deriving Show

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

-- Having both SContext and the ValidForContext entity in the engine fxn proves convenient
-- For now, we straight blow up if c is not valid for its own derived context.
withContext :: (CanBeValid c, HasContext c) => c -> (forall (a :: Asset) m n. (SContext a m n, ValidForContext a m n c) -> r) -> r
withContext c f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) -> let
      ctx = SContext s (P2 @m @n)
    in
    f (ctx, fromJust $ mkSafe ctx c)
  where
    a = getAsset c
    i = getXDim c
    j = getYDim c

data ValidForContext (a :: Asset) (m :: Nat) (n :: Nat) (s :: *) = ValidForContext (SAsset a) (ValidForPlane m n s) deriving (Functor)
instance (Show s) => Show (ValidForContext a m n s) where
  show = show <<< unwrapValidContext
instance CoordinateLike s => CoordinateLike (ValidForContext a m n s) where
  getX = getX <<< unwrapValidContext
  getY = getY <<< unwrapValidContext

instance PlaneLike (ValidForContext a m n s) where
  getXDim (ValidForContext _ p)= getXDim p
  getYDim (ValidForContext _ p)= getYDim p
instance HasContext (ValidForContext a m n s) where
  getAsset (ValidForContext a _)= fromSing a

unsafeMkValid :: (KnownNat m, KnownNat n) => s -> SAsset a -> ValidForContext a m n s
unsafeMkValid s a = ValidForContext a (ValidForPlane s)

unwrapValidContext :: ValidForContext a m n s -> s
unwrapValidContext (ValidForContext _ (ValidForPlane s)) = s

class CanBeValid (c :: * ) where
  mkSafe :: SContext a m n -> c -> Maybe (ValidForContext a m n c)
instance CanBeValid PaintingRecord where
  mkSafe c@(SContext a p) painting =
    if assetValid && planeValid
    then Just $ ValidForContext a (ValidForPlane painting)
    else Nothing
    where
      assetValid = getAsset c == paintingRecordAsset painting
      planeValid = getXDim p == fromIntegral (paintingRecordXSize painting) && getYDim p == fromIntegral (paintingRecordYSize painting)
instance CanBeValid (Entity PaintingRecord) where
  mkSafe c@(SContext a _) ep = case mkSafe c (entityVal ep) of
    Nothing -> Nothing
    Just _ -> pure $ ValidForContext a (ValidForPlane ep)

getVertexRecordId :: VertexRecord -> VertexRecordId
getVertexRecordId VertexRecord{..} =
  VertexRecordKey
    vertexRecordPaintingRecordId
    vertexRecordX
    vertexRecordY

instance CoordinateLike LocaleRecord where
  getX = fromIntegral <<< localeRecordX
  getY = fromIntegral <<< localeRecordY

instance CoordinateLike VertexRecord where
  getX = fromIntegral <<< vertexRecordX
  getY = fromIntegral <<< vertexRecordY

instance CoordinateLike VertexRecordId where
  getX (VertexRecordKey _ x _) = fromIntegral x
  getY (VertexRecordKey _ _ y) = fromIntegral y

instance PlaneLike PaintingRecord where
  getXDim = fromIntegral <<< paintingRecordXSize
  getYDim = fromIntegral <<< paintingRecordYSize

instance HasContext PaintingRecord where
  getAsset = paintingRecordAsset

instance PlaneLike (Entity PaintingRecord) where
  getXDim = getXDim <<< entityVal
  getYDim = getYDim <<< entityVal
instance HasContext (Entity PaintingRecord) where
  getAsset = getAsset <<< entityVal

getVertexRecord :: VertexRecordApproximation a m n -> ValidForPlane m n VertexRecord
getVertexRecord (PerfectRecord v _) = v
getVertexRecord (ApproximateRecord v _) = v
getVertexRecord (NoRecord v ) = v

data VertexRecordApproximation (a :: Asset) (m :: Nat) (n :: Nat) where
  PerfectRecord :: ValidForPlane m n VertexRecord -> ValidForContext a m n LocaleRecord -> VertexRecordApproximation a m n
  ApproximateRecord :: ValidForPlane m n VertexRecord -> ValidForContext a m n LocaleRecord -> VertexRecordApproximation a m n
  NoRecord :: ValidForPlane m n VertexRecord -> VertexRecordApproximation a m n
instance Show (VertexRecordApproximation a m n) where
  show = show <<< unwrapValid <<< getVertexRecord


data ApproximationQuality = Perfect | Approx Natural | None deriving Eq
getApproximationQuality :: VertexRecordApproximation a m n -> ApproximationQuality
getApproximationQuality (PerfectRecord _ _) = Perfect
getApproximationQuality (ApproximateRecord v l) = Approx <<$ l1Dist v l
getApproximationQuality (NoRecord _) = None


instance CoordinateLike (VertexRecordApproximation a m n) where
  getX = getX <<< getVertexRecord
  getY = getY <<< getVertexRecord
