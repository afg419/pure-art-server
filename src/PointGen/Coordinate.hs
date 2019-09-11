{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module PointGen.Coordinate where

import PointGen.Plane
import Import
import Data.Aeson

type KnownNats m n = (KnownNat m, KnownNat n)

type Coordinate2 = (Integer, Integer)

data SCoordinate2 (m :: Nat) (n :: Nat) where
  SCoordinate2 :: forall m n. KnownNats m n =>
    { x :: Integer, y :: Integer } -> SCoordinate2 m n

instance Show (SCoordinate2 m n) where
  show c@SCoordinate2{..} = pack $ "(x:" <> show x <> " y:"<> show y <> " plane:" <> show xDim <> "," <> show yDim <> ")"
    where
      (xDim, yDim) = plane2Dim' c
deriving instance Eq (SCoordinate2 m n)

instance Ord (SCoordinate2 m n) where -- instance used for nubbing localeRecords in O(n*log(n)) time
  SCoordinate2 x1 y1 <= SCoordinate2 x2 y2 =
    if x1 <= x2
      then y1 <= y2
      else False

l1Dist :: SCoordinate2 m n -> SCoordinate2 m n -> Integer
l1Dist c1 c2 = abs (x c1 - x c2) + abs (y c1 - y c2)

mkCoordinate :: forall m n f. KnownNats m n => Integer -> Integer -> f m n -> Maybe (SCoordinate2 m n)
mkCoordinate x' y' c = if xValid && yValid
  then Just $ SCoordinate2 x' y'
  else Nothing
  where
    (xSize, ySize) = plane2Dim' c
    xValid = 0 <= x' && x' <= xSize - 1
    yValid = 0 <= y' && y' <= ySize - 1
