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

class CTY s where
  ctyAsset :: s -> Asset
  ctyX :: Integral a => s -> a
  ctyY :: Integral a => s -> a

ctyEq :: (CTY s, CTY r) => r -> s -> Bool
ctyEq r s = (ra, rx, ry) == (sa, sx, sy)
  where
    ra = ctyAsset r
    rx = ctyX r :: Integer
    ry = ctyY r :: Integer

    sa = ctyAsset s
    sx = ctyX s :: Integer
    sy = ctyY s :: Integer

data SCTY (a :: Asset) (m :: Nat) (n :: Nat) where
  SCTY :: (KnownNat m, KnownNat n) =>
    { sAsset :: SAsset a
    , dim :: Plane2 m n
    } -> SCTY a m n

instance CTY (SCTY a m n) where
  ctyAsset = fromSing <<< sAsset
  ctyX = fromIntegral <<< fst <<< dimensionsSCTY
  ctyY = fromIntegral <<< snd <<< dimensionsSCTY

data Safe (s :: *) (a :: Asset) (m :: Nat) (n :: Nat) = Safe s
fromSafe :: Safe s a m n -> s
fromSafe (Safe s) = s

dimensionsSCTY :: SCTY a m n -> (Natural, Natural)
dimensionsSCTY (SCTY _ p) = dimensions p

withCanvasTy :: CTY cty => cty -> (forall (a :: Asset) m n. (SCTY a m n, Safe cty a m n) -> r) -> r
withCanvasTy cty f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ (SCTY s (P2 @m @n), Safe cty)
  where
    a = ctyAsset cty
    i = ctyX cty
    j = ctyY cty
