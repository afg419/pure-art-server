{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.PointGen where

import Test.QuickCheck
import PointGen
import Import hiding (undefined)
import qualified Basement.Types.Word256 as B
import qualified Basement.Numerical.Number as B
import Data.Maybe

stockLeqThan :: (KnownNat m, KnownNat n) => PlaneStock -> Plane2 m n -> Bool
stockLeqThan ps targetPlane = withPlaneStock ps (`leqDimensionsThan` targetPlane)

instance Arbitrary PlaneStock where
  arbitrary = arbitraryBoundedEnum

withRandomPlane :: (PlaneStock -> Bool) -> (forall m n. KnownNats m n => Plane2 m n -> Gen s) -> Gen s
withRandomPlane planeHasProperty s = do
  stock <- arbitrary `suchThat` planeHasProperty
  withPlaneStock stock s

withRandomCoordinate :: (forall m n. KnownNats m n => Coordinate2 m n -> Gen s) -> Gen s
withRandomCoordinate s = do
  stock <- arbitrary
  withPlaneStock stock $ genCoordinate >=> s

genCoordinate :: KnownNats m n => Plane2 m n -> Gen (Coordinate2 m n)
genCoordinate targetPlane = fmap (projectTo targetPlane) $ genFibreCoordinate

genSubplaneCoordinate :: KnownNats m n => SubPlane2 m n -> Gen (Coordinate2 m n)
genSubplaneCoordinate sp = do
  let (dimX, dimY) = subplane2Dim sp
  let (shiftX, shiftY) = subplane2Translate sp

  genXOrigin <- arbitrary `suchThat` (\i -> 0 <= i && i < dimX)
  genYOrigin <- arbitrary `suchThat` (\i -> 0 <= i && i < dimY)

  let genX = genXOrigin + shiftX
  let genY = genYOrigin + shiftY

  pure <<< fromJust $ (mkCoordinate genX genY P2) >>= bToM (`inSubPlane` sp)

genFibreCoordinate :: Gen FibreCoordinate
genFibreCoordinate = do
  w64_1_x <- arbitrary
  w64_2_x <- arbitrary
  w64_3_x <- arbitrary
  w64_4_x <- arbitrary

  let genx = B.toInteger $ B.Word256 w64_1_x w64_2_x w64_3_x w64_4_x

  w64_1_y <- arbitrary
  w64_2_y <- arbitrary
  w64_3_y <- arbitrary
  w64_4_y <- arbitrary

  let geny = B.toInteger $ B.Word256 w64_1_y w64_2_y w64_3_y w64_4_y
  pure <<< fromJust $ mkCoordinate genx geny fibrePlane
