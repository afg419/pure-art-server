{-# LANGUAGE ConstraintKinds #-}

module PointGen.PlaneTransform where

import PointGen.Plane
import PointGen.Range
import PointGen.Coordinate
import Data.Maybe
import Import

projectTo :: forall n1 m1 n2 m2. Plane m2 n2 -> SCoordinate m1 n1 -> SCoordinate m2 n2
projectTo cNew@P2 c =
  fromJust $ mkSafeCoordinate P2 $ C xScaled yScaled
  where
    x' = getX c
    y' = getY c
    (xNew, yNew) = dimensions cNew
    (xOld, yOld) = planeDimensions c
    xScaled = (x' * xNew) `div` xOld
    yScaled = (y' * yNew) `div` yOld

fibreOver :: forall m1 n1 m2 n2. Plane m1 n1 -> SCoordinate m2 n2 -> SubPlane2 m1 n1
fibreOver plane_up@P2 (ValidForPlane (C x' y'))=
  SubPlane2 (ValidForPlane $ C botX botY) (ValidForPlane $ C topX topY)
  where
    plane_down = P2 @m2 @n2

    (plane_up_X, plane_up_Y) = dimensions plane_up
    (plane_down_X, plane_down_Y) = dimensions plane_down

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
  SubPlane2 :: SCoordinate m n -> SCoordinate m n -> SubPlane2 m n
deriving instance (Eq (SubPlane2 m n))
deriving instance (Show (SubPlane2 m n))

xRange :: SubPlane2 m n -> Range Natural
xRange (SubPlane2 c1 c2) = mkRange (getX c1) (getX c2)

yRange :: SubPlane2 m n -> Range Natural
yRange (SubPlane2 (ValidForPlane (C _ y1)) (ValidForPlane (C _ y2))) = mkRange y1 y2

-- subplane length + width
subplane2Dim :: SubPlane2 m n -> (Natural, Natural)
subplane2Dim = (diameter <<< xRange) &&& (diameter <<< xRange)

-- subplane is translated from origin by this amount
subplane2Translate :: SubPlane2 m n -> (Natural, Natural)
subplane2Translate = (low <<< xRange) &&& (low <<< yRange)

inSubPlane :: SCoordinate m n -> SubPlane2 m n -> Bool
inSubPlane c sp = (getX c `inRange` xRange sp) && (getY c `inRange` yRange sp)

scaleToNewSegment :: Natural -> Natural -> Natural -> Natural
scaleToNewSegment initialCount finalCount initialPoint =
  (initialPoint * finalCount) `div` initialCount
