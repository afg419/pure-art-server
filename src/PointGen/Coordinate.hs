{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

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

class TwoDimensional c where
  getX :: c -> Natural
  getY :: c -> Natural

l1Dist :: (TwoDimensional c, TwoDimensional d) => c -> d -> Natural
l1Dist c d = abs (getX c - getX d) + abs (getY c - getY d)


instance TwoDimensional Coordinate2 where
  getX = fst
  getY = snd

instance TwoDimensional (SCoordinate2 m n) where
  getX = cx
  getY = cy

mkCoordinate :: forall m n. Natural -> Natural -> Plane2 m n -> Maybe (SCoordinate2 m n)
mkCoordinate x' y' P2 = if xValid && yValid
  then Just $ SCoordinate2 x' y'
  else Nothing
  where
    (xSize, ySize) = dimensions (P2 @m @n)
    xValid = 0 <= x' && x' <= xSize `minusNatural` 1
    yValid = 0 <= y' && y' <= ySize `minusNatural` 1
