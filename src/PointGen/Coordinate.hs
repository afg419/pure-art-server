
module PointGen.Coordinate where

import PointGen.Canvas
import PointGen.Range
import Data.Maybe
import Import

data Coordinate (m :: Nat) (n :: Nat) where
  Coordinate :: forall m n. (KnownNat m, KnownNat n) =>
    { x :: Integer, y :: Integer, canvas :: Canvas2 m n } -> Coordinate m n

deriving instance Show (Coordinate m n)

mkCoordinate :: forall m n. (KnownNat m, KnownNat n) => Integer -> Integer -> Canvas2 m n -> Maybe (Coordinate m n)
mkCoordinate x' y' c = if xValid && yValid
  then Just $ Coordinate x' y' c
  else Nothing
  where
    (xSize, ySize) = canvas2Dim c
    xValid = 0 <= x' && x' <= xSize - 1
    yValid = 0 <= y' && y' <= ySize - 1

scaleToNewCanvas :: (KnownNat m1, KnownNat n1, KnownNat m2, KnownNat n2)
  => Canvas2 m2 n2 -> Coordinate m1 n1 -> Coordinate m2 n2
scaleToNewCanvas cNew (Coordinate x' y' cOld) =
  fromJust $ mkCoordinate xScaled yScaled cNew
  where
    (xNew, yNew) = canvas2Dim cNew
    (xOld, yOld) = canvas2Dim cOld
    xScaled = (x' * xNew) `div` xOld
    yScaled = (y' * yNew) `div` yOld

data SubCanvas2 (m :: Nat) (n :: Nat)  where
  SubCanvas2 :: Coordinate m n -> Coordinate m n -> SubCanvas2 m n

getYRange :: SubCanvas2 m n -> Range Integer
getYRange (SubCanvas2 (Coordinate _ y1 _) (Coordinate _ y2 _)) = mkRange y1 y2

getXRange :: SubCanvas2 m n -> Range Integer
getXRange (SubCanvas2 (Coordinate x1 _ _) (Coordinate x2 _ _)) = mkRange x1 x2

preimageInCanvas :: (KnownNat m1, KnownNat n1, KnownNat m2, KnownNat n2) =>
  Canvas2 m1 n1 -> Coordinate m2 n2 -> SubCanvas2 m1 n1
preimageInCanvas preCanvas (Coordinate x' y' targetCanvas) =
  SubCanvas2 (Coordinate botX botY preCanvas) (Coordinate topX topY preCanvas)
  where
    (preCanvasX, preCanvasY) = canvas2Dim preCanvas
    (targetCanvasX, targetCanvasY) = canvas2Dim targetCanvas

    botX = scaleToNewSegment targetCanvasX preCanvasX x'
    botY = scaleToNewSegment targetCanvasY preCanvasY y'

    topX = min (1 + botX) preCanvasX
    topY = min (1 + botY) preCanvasY

scaleToNewSegment :: Integer -> Integer -> Integer -> Integer
scaleToNewSegment initialCount finalCount initialPoint =
  initialPoint * finalCount `div` initialCount
