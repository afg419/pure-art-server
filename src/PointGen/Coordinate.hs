{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module PointGen.Coordinate where

import PointGen.Plane
import PointGen.Range
import Data.Maybe
import Import

type KnownNats m n = (KnownNat m, KnownNat n)

data Coordinate2 (m :: Nat) (n :: Nat) where
  Coordinate2 :: forall m n. KnownNats m n =>
    { x :: Integer, y :: Integer, plane :: Plane2 m n } -> Coordinate2 m n

instance Show (Coordinate2 m n) where
  show Coordinate2{..} = pack $ "(x:" <> show x <> " y:"<> show y <> " plane:" <> show xDim <> "," <> show yDim <> ")"
    where
      (xDim, yDim) = plane2Dim plane


deriving instance Eq (Coordinate2 m n)

mkCoordinate :: forall m n. (KnownNat m, KnownNat n) => Integer -> Integer -> Plane2 m n -> Maybe (Coordinate2 m n)
mkCoordinate x' y' c = if xValid && yValid
  then Just $ Coordinate2 x' y' c
  else Nothing
  where
    (xSize, ySize) = plane2Dim c
    xValid = 0 <= x' && x' <= xSize - 1
    yValid = 0 <= y' && y' <= ySize - 1

projectTo :: (KnownNats m1 n1, KnownNats m2 n2)
  => Plane2 m2 n2 -> Coordinate2 m1 n1 -> Coordinate2 m2 n2
projectTo cNew (Coordinate2 x' y' cOld) =
  fromJust $ mkCoordinate xScaled yScaled cNew
  where
    (xNew, yNew) = plane2Dim cNew
    (xOld, yOld) = plane2Dim cOld
    xScaled = (x' * xNew) `div` xOld
    yScaled = (y' * yNew) `div` yOld

data SubPlane2 (m :: Nat) (n :: Nat)  where
  SubPlane2 :: Coordinate2 m n -> Coordinate2 m n -> SubPlane2 m n
deriving instance (Eq (SubPlane2 m n))
deriving instance (Show (SubPlane2 m n))

-- subplane length + width
subplane2Dim :: SubPlane2 m n -> (Integer, Integer)
subplane2Dim = (diameter <<< getXRange) &&& (diameter <<< getXRange)

-- subplane is translated from origin by this amount
subplane2Translate :: SubPlane2 m n -> (Integer, Integer)
subplane2Translate = (low <<< getXRange) &&& (low <<< getYRange)

getYRange :: SubPlane2 m n -> Range Integer
getYRange (SubPlane2 (Coordinate2 _ y1 _) (Coordinate2 _ y2 _)) = mkRange y1 y2

getXRange :: SubPlane2 m n -> Range Integer
getXRange (SubPlane2 (Coordinate2 x1 _ _) (Coordinate2 x2 _ _)) = mkRange x1 x2

inSubPlane :: Coordinate2 m n -> SubPlane2 m n -> Bool
inSubPlane c sp = (x c) `inRange` xRange && (y c) `inRange` yRange
  where
    xRange = getXRange sp
    yRange = getYRange sp




fibreOver :: (KnownNat m1, KnownNat n1, KnownNat m2, KnownNat n2) =>
  Plane2 m1 n1 -> Coordinate2 m2 n2 -> SubPlane2 m1 n1
fibreOver plane_up (Coordinate2 x' y' plane_down) =
  SubPlane2 (Coordinate2 botX botY plane_up) (Coordinate2 topX topY plane_up)
  where
    (plane_up_X, plane_up_Y) = plane2Dim plane_up
    (plane_down_X, plane_down_Y) = plane2Dim plane_down

    botX = scaleToNewSegment plane_down_X plane_up_X x'
    botY = scaleToNewSegment plane_down_Y plane_up_Y y'

    topX' = scaleToNewSegment plane_down_X plane_up_X (x' + 1)
    topY' = scaleToNewSegment plane_down_Y plane_up_Y (y' + 1)

    topX = min (topX' - 1) (plane_up_X - 1)
    topY = min (topY' - 1) (plane_up_Y - 1)

scaleToNewSegment :: Integer -> Integer -> Integer -> Integer
scaleToNewSegment initialCount finalCount initialPoint =
  (initialPoint * finalCount) `div` initialCount
