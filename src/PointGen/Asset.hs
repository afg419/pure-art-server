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


data CTY (a :: Asset) (m :: Nat) (n :: Nat) where
  CTY :: (KnownNat m, KnownNat n) =>
    { sAsset :: SAsset a
    , dim :: Plane2 m n
    } -> CTY a m n

dimensionCTY :: CTY a m n -> (Natural, Natural)
dimensionCTY (CTY _ p) = dimensions p

withCanvasTy :: (Asset, Natural, Natural) -> (forall (a :: Asset) m n. CTY a m n -> r) -> r
withCanvasTy (a, i, j) f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ CTY s (P2 @m @n)
