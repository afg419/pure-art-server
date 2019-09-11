{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}

module PointGen.PlaneTransform where

import PointGen.Plane
import PointGen.Range
import PointGen.Coordinate
import Data.Maybe
import Import

projectTo :: (KnownNats m1 n1, KnownNats m2 n2)
  => Plane2 m2 n2 -> SCoordinate2 m1 n1 -> SCoordinate2 m2 n2
projectTo cNew c@(SCoordinate2 x' y') =
  fromJust $ mkCoordinate xScaled yScaled cNew
  where
    (xNew, yNew) = plane2Dim cNew
    (xOld, yOld) = plane2Dim' c
    xScaled = (x' * xNew) `div` xOld
    yScaled = (y' * yNew) `div` yOld

fibreOver :: (KnownNat m1, KnownNat n1, KnownNat m2, KnownNat n2) =>
  Plane2 m1 n1 -> SCoordinate2 m2 n2 -> SubPlane2 m1 n1
fibreOver plane_up c@(SCoordinate2 x' y') =
  SubPlane2 (SCoordinate2 botX botY) (SCoordinate2 topX topY)
  where
    plane_down = plane2For c

    (plane_up_X, plane_up_Y) = plane2Dim plane_up
    (plane_down_X, plane_down_Y) = plane2Dim plane_down

    botX = scaleToNewSegment plane_down_X plane_up_X x'
    botY = scaleToNewSegment plane_down_Y plane_up_Y y'

    topX' = scaleToNewSegment plane_down_X plane_up_X (x' + 1)
    topY' = scaleToNewSegment plane_down_Y plane_up_Y (y' + 1)

    topX = min (topX' - 1) (plane_up_X - 1)
    topY = min (topY' - 1) (plane_up_Y - 1)

--------------------------------------------------------------------------------
-- SubPlanes
--------------------------------------------------------------------------------

-- Coordinates represent opposite corners of the plane
data SubPlane2 (m :: Nat) (n :: Nat)  where
  SubPlane2 :: SCoordinate2 m n -> SCoordinate2 m n -> SubPlane2 m n
deriving instance (Eq (SubPlane2 m n))
deriving instance (Show (SubPlane2 m n))

xRange :: SubPlane2 m n -> Range Integer
xRange (SubPlane2 (SCoordinate2 x1 _) (SCoordinate2 x2 _ )) = mkRange x1 x2

yRange :: SubPlane2 m n -> Range Integer
yRange (SubPlane2 (SCoordinate2 _ y1) (SCoordinate2 _ y2)) = mkRange y1 y2

-- subplane length + width
subplane2Dim :: SubPlane2 m n -> (Integer, Integer)
subplane2Dim = (diameter <<< xRange) &&& (diameter <<< xRange)

-- subplane is translated from origin by this amount
subplane2Translate :: SubPlane2 m n -> (Integer, Integer)
subplane2Translate = (low <<< xRange) &&& (low <<< yRange)

inSubPlane :: SCoordinate2 m n -> SubPlane2 m n -> Bool
inSubPlane c sp = (x c) `inRange` (xRange sp) && (y c) `inRange` (yRange sp)

scaleToNewSegment :: Integer -> Integer -> Integer -> Integer
scaleToNewSegment initialCount finalCount initialPoint =
  (initialPoint * finalCount) `div` initialCount
