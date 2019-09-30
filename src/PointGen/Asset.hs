{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module PointGen.Asset where

import PointGen.Plane
import Data.Singletons.TH
import Database.Persist.Sql
import Import
import GHC.TypeNats
import Data.Aeson

$(singletons [d|
  data Asset = DOGE
    deriving (Show, Eq)
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

instance ToJSON Asset where
    toJSON DOGE = String "DOGE"

instance FromJSON Asset where
  parseJSON = withText "asset" $ \t -> do

    if t == "DOGE"
      then pure DOGE
      else fail (unpack t)
parseAsset :: Text -> Maybe Asset
parseAsset = headMay <<< preimage' (pack <<< show)

instance PersistField Asset where
  toPersistValue = PersistText <<< pack <<< show
  fromPersistValue (PersistText t) = mToE "Invalid Asset" <<< parseAsset $ t
  fromPersistValue _ = Left $ "Expected Asset saved as text"
instance PersistFieldSql Asset where
  sqlType _ = SqlString

class TwoDimensional s => CTY s where
  getAsset :: s -> Asset

ctyEq :: (CTY s, CTY r) => r -> s -> Bool
ctyEq r s = (ra, rx, ry) == (sa, sx, sy)
  where
    ra = getAsset r
    rx = toInteger <<$ getX r
    ry = toInteger <<$ getY r

    sa = getAsset s
    sx = toInteger <<$ getX s
    sy = toInteger <<$ getY s

data SCTY (a :: Asset) (m :: Nat) (n :: Nat) where
  SCTY :: (KnownNat m, KnownNat n) =>
    { sAsset :: SAsset a
    , dim :: Plane2 m n
    } -> SCTY a m n

instance TwoDimensional (SCTY a m n) where
  getX = getX <<< dim
  getY = getY <<< dim

instance CTY (SCTY a m n) where
  getAsset = fromSing <<< sAsset

dimensionsSCTY :: SCTY a m n -> (Natural, Natural)
dimensionsSCTY (SCTY _ p) = dimensions p

withCanvasTy :: CTY cty => cty -> (forall (a :: Asset) m n. (SCTY a m n, SafeCTY a m n cty) -> r) -> r
withCanvasTy cty f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ (SCTY s (P2 @m @n), toSafeCTY cty)
  where
    a = getAsset cty
    i = getX cty
    j = getY cty

data SafeCTY (a :: Asset) (m :: Nat) (n :: Nat) (s :: *) = SafeCTY (Safe2D m n s) deriving (Functor)

instance TwoDimensional s => TwoDimensional (Safe2D m n s) where
  getX = getX <<< fromSafe2D
  getY = getY <<< fromSafe2D

instance TwoDimensional s => TwoDimensional (SafeCTY a m n s) where
  getX = getX <<< fromSafeCTY
  getY = getY <<< fromSafeCTY

instance CTY s => CTY (SafeCTY a m n s) where
  getAsset = getAsset <<< fromSafeCTY

onSafeCTY :: (s -> t) -> SafeCTY a m n s -> t
onSafeCTY f = fromSafeCTY <<< fmap f

onSafe2D :: (s -> t) -> Safe2D m n s -> t
onSafe2D f = fromSafe2D <<< fmap f

toSafeCTY :: s -> SafeCTY a m n s
toSafeCTY = Safe2D >>> SafeCTY

fromSafeCTY :: SafeCTY a m n s -> s
fromSafeCTY (SafeCTY (Safe2D s)) = s

fromSafe2D :: Safe2D m n s -> s
fromSafe2D (Safe2D s) = s

data Safe2D (m :: Nat) (n :: Nat) (s :: *)  = Safe2D s deriving (Functor)

instance Show s => Show (Safe2D m n s) where
  show (Safe2D s) = show s

mkSafeCTY :: CTY s => TwoDimensionalCheck -> SCTY a m n -> s -> Maybe (SafeCTY a m n s)
mkSafeCTY c scty s = mkSafe2D c (dim scty) s >>=
    \sD2 -> if getAsset scty == getAsset s
      then Just (SafeCTY sD2)
      else Nothing

data TwoDimensionalCheck = WithinRange | Equals

mkSafe2D :: TwoDimensional s => TwoDimensionalCheck -> Plane2 m n -> s -> Maybe (Safe2D m n s)
mkSafe2D WithinRange p2 d2 = if xValid && yValid
  then Just $ Safe2D d2
  else Nothing
  where
    (sizeX, sizeY) = dimensions p2
    xValid = 0 <= getX d2 && getX d2 <= sizeX `minusNatural` 1
    yValid = 0 <= getY d2 && getY d2 <= sizeY `minusNatural` 1
mkSafe2D Equals p2 d2 = if xValid && yValid
  then Just $ Safe2D d2
  else Nothing
  where
    (sizeX, sizeY) = dimensions p2
    xValid = getX d2 == sizeX
    yValid = getY d2 == sizeY
