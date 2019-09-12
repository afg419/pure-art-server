{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module PointGen.Coordinate where

import PointGen.Plane
import Import

type KnownNats m n = (KnownNat m, KnownNat n)

type Coordinate2 = (Natural, Natural)

data SCoordinate2 (m :: Nat) (n :: Nat) where
  SCoordinate2 :: KnownNats m n => { cx :: Natural, cy :: Natural } -> SCoordinate2 m n

instance Show (SCoordinate2 m n) where
  show SCoordinate2{..} = pack $ "(x:" <> show cx <> ", y:"<> show cy <>")"
deriving instance Eq (SCoordinate2 m n)

instance Ord (SCoordinate2 m n) where -- instance used for nubbing localeRecords in O(n*log(n)) time
  SCoordinate2 x1 y1 <= SCoordinate2 x2 y2 =
    if x1 <= x2
      then y1 <= y2
      else False

l1Dist :: SCoordinate2 m n -> SCoordinate2 m n -> Natural
l1Dist c1 c2 = abs (cx c1 - cx c2) + abs (cy c1 - cy c2)

mkCoordinate :: forall m n. Natural -> Natural -> Plane2 m n -> Maybe (SCoordinate2 m n)
mkCoordinate x' y' P2 = if xValid && yValid
  then Just $ SCoordinate2 x' y'
  else Nothing
  where
    (xSize, ySize) = dimensions (P2 @m @n)
    xValid = 0 <= x' && x' <= xSize `minusNatural` 1
    yValid = 0 <= y' && y' <= ySize `minusNatural` 1
