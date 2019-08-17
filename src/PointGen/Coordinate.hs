{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module PointGen.Coordinate where

import PointGen.Plane
import Import
import Data.Aeson

type KnownNats m n = (KnownNat m, KnownNat n)

data Coordinate2 (m :: Nat) (n :: Nat) where
  Coordinate2 :: forall m n. KnownNats m n =>
    { x :: Integer, y :: Integer, plane :: Plane2 m n } -> Coordinate2 m n

instance KnownNats m n => FromJSON (Coordinate2 m n) where
  parseJSON = withObject "coordinate 2" $ \o -> do
    xCoordinate <- o .: "x"
    yCoordinate <- o .: "y"
    let p = P2
    let (xSize, ySize) = plane2Dim p
    if (0 > xCoordinate || xCoordinate > xSize) || (0 > yCoordinate || yCoordinate > ySize)
      then fail "coordinate out of plane bounds"
      else pure <<$ Coordinate2 xCoordinate yCoordinate p

instance Show (Coordinate2 m n) where
  show Coordinate2{..} = pack $ "(x:" <> show x <> " y:"<> show y <> " plane:" <> show xDim <> "," <> show yDim <> ")"
    where
      (xDim, yDim) = plane2Dim plane
deriving instance Eq (Coordinate2 m n)

instance Ord (Coordinate2 m n) where -- instance used for nubbing localeRecords in O(n*log(n)) time
  Coordinate2 x1 y1 _ <= Coordinate2 x2 y2 _ =
    if x1 <= x2
      then y1 <= y2
      else False

mkCoordinate :: forall m n. KnownNats m n => Integer -> Integer -> Plane2 m n -> Maybe (Coordinate2 m n)
mkCoordinate x' y' c = if xValid && yValid
  then Just $ Coordinate2 x' y' c
  else Nothing
  where
    (xSize, ySize) = plane2Dim c
    xValid = 0 <= x' && x' <= xSize - 1
    yValid = 0 <= y' && y' <= ySize - 1
