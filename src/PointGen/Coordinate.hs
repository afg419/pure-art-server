{-# LANGUAGE ConstraintKinds #-}

module PointGen.Coordinate where

import PointGen.Plane
import PointGen.Range
import Data.Maybe
import Import

type KnownNats m n = (KnownNat m, KnownNat n)

data Coordinate2 (m :: Nat) (n :: Nat) where
  Coordinate2 :: forall m n. KnownNats m n =>
    { x :: Integer, y :: Integer, plane :: Plane2 m n } -> Coordinate2 m n

deriving instance Show (Coordinate2 m n)
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
fibreOver prePlane (Coordinate2 x' y' targetPlane) =
  SubPlane2 (Coordinate2 botX botY prePlane) (Coordinate2 topX topY prePlane)
  where
    (prePlaneX, prePlaneY) = plane2Dim prePlane
    (targetPlaneX, targetPlaneY) = plane2Dim targetPlane

    botX = scaleToNewSegment targetPlaneX prePlaneX x'
    botY = scaleToNewSegment targetPlaneY prePlaneY y'

    topX = min (1 + botX) prePlaneX
    topY = min (1 + botY) prePlaneY

scaleToNewSegment :: Integer -> Integer -> Integer -> Integer
scaleToNewSegment initialCount finalCount initialPoint =
  initialPoint * finalCount `div` initialCount