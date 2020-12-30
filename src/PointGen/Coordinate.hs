{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module PointGen.Coordinate where

import PointGen.Plane
import Import

data Coordinate = C Natural Natural deriving (Show, Eq)
instance FromJSON Coordinate where
  parseJSON v = uncurry C <$> parseJSON v

-- instance used for nubbing localeRecords in O(n*log(n)) time
-- this is not a total ordering, as p1 <= p2 and p2 <= p1 may be false.
instance Ord Coordinate where
   C x1 y1 <=  C x2 y2 = (x1 <= x2) && (y1 <= y2)

type SCoordinate (m :: Nat) (n :: Nat) = ValidForPlane m n Coordinate

mkSafeCoordinate :: CoordinateLike s => Plane m n -> s -> Maybe (ValidForPlane m n s)
mkSafeCoordinate p2@P2 d2 = if xValid && yValid
  then Just $ ValidForPlane d2
  else Nothing
  where
    (sizeX, sizeY) = dimensions p2
    xValid = 0 <= getX d2 && getX d2 <= sizeX `minusNatural` 1
    yValid = 0 <= getY d2 && getY d2 <= sizeY `minusNatural` 1


class CoordinateLike c where
  getX :: c -> Natural
  getY :: c -> Natural

instance CoordinateLike Coordinate where
  getX (C x _) = x
  getY (C _ y) = y

-- Note: the x and y returned are the x and y of s, not m and n of the plane.
-- planeLike instance for ValidForPlane gets m and n.
instance CoordinateLike s => CoordinateLike (ValidForPlane m n s) where
  getX = getX <<< unwrapValid
  getY = getY <<< unwrapValid

-- l1 distance between two dimensional coordinates
l1Dist :: (CoordinateLike c, CoordinateLike d) => c -> d -> Natural
l1Dist c d = fromIntegral <<$ (debug "first" $ abs (getXInt c - getXInt d)) + (debug "second" $ abs (getYInt c - getYInt d))
  where
    getXInt :: CoordinateLike e => e -> Integer
    getXInt  = toInteger <<< getX
    getYInt :: CoordinateLike e => e -> Integer
    getYInt = toInteger <<< getY
