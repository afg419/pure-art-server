{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module PointGen.Asset where

import PointGen.Plane
import Data.Singletons.TH
import Database.Persist.Sql
import Import
import GHC.TypeNats

$(singletons [d|
  data Asset = DOGE
    deriving (Show, Eq, Generic)
  |])

someAsset :: Asset -> SomeSing Asset
someAsset DOGE = SomeSing SDOGE

instance Enum Asset where
  fromEnum DOGE = 0
  toEnum 0 = DOGE
  toEnum _ = DOGE

instance Bounded Asset where
  minBound = DOGE
  maxBound = DOGE

instance ToJSON Asset
instance FromJSON Asset

parseAsset :: Text -> Maybe Asset
parseAsset = headMay <<< preimage' (pack <<< show)

instance PersistField Asset where
  toPersistValue = PersistText <<< pack <<< show
  fromPersistValue (PersistText t) = mToE "Invalid Asset" <<< parseAsset $ t
  fromPersistValue _ = Left $ "Expected Asset saved as text"
instance PersistFieldSql Asset where
  sqlType _ = SqlString

data CTY = CTY Asset Natural Natural deriving Eq

data SCTY (a :: Asset) (m :: Nat) (n :: Nat) where
  SCTY :: (KnownNat m, KnownNat n) =>
    { sAsset :: SAsset a
    , dim :: Plane2 m n
    } -> SCTY a m n

demoteSCTY :: SCTY a m n -> CTY
demoteSCTY scty = CTY (fromSing <<< sAsset <<$ scty) xSize ySize
  where
    (xSize, ySize) = dimensionsSCTY scty

dimensionsSCTY :: SCTY a m n -> (Natural, Natural)
dimensionsSCTY (SCTY _ p) = dimensions p

dimensionsCTY :: CTY -> (Natural, Natural)
dimensionsCTY (CTY _ m n) = (m,n)

withCanvasTy :: CTY -> (forall (a :: Asset) m n. SCTY a m n -> r) -> r
withCanvasTy (CTY a i j) f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ SCTY s (P2 @m @n)
