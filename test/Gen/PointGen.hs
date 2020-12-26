{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.PointGen where

import Test.QuickCheck
import PointGen
import Import hiding (undefined)
import qualified Basement.Types.Word256 as B
import qualified Basement.Numerical.Number as B
import Data.Maybe

instance Arbitrary Natural where
  arbitrary = fmap B.toNatural <<$ (arbitrary :: Gen Word64)

stockLeqThan :: (Natural, Natural) -> Plane m n -> Bool
stockLeqThan (x,y) targetPlane = withPlaneTy (x,y) (`leqDimensionsThan` targetPlane)

withRandomPlane :: ((Natural, Natural) -> Bool) -> (forall m n. Plane m n -> Gen s) -> Gen s
withRandomPlane xyHasProp s = do
  (x, y) <- arbitrary `suchThat` xyHasProp
  withPlaneTy (x, y) s

withRandomCoordinate :: (forall m n. SCoordinate m n -> Gen s) -> Gen s
withRandomCoordinate s = withRandomPlane (const True) (genCoordinate >=> s)

genCoordinate :: Plane m n -> Gen (SCoordinate m n)
genCoordinate targetPlane = fmap (projectTo targetPlane) $ genFibreCoordinate

genSubplaneCoordinate :: forall m n. SubPlane2 m n -> Gen (SCoordinate m n)
genSubplaneCoordinate sp@(SubPlane2 (SCoordinate _ _) _) = do
  let (dimX, dimY) = subplane2Dim sp
  let (shiftX, shiftY) = subplane2Translate sp

  genXOrigin <- arbitrary `suchThat` (\i -> 0 <= i && i < dimX)
  genYOrigin <- arbitrary `suchThat` (\i -> 0 <= i && i < dimY)

  let genX = genXOrigin + shiftX
  let genY = genYOrigin + shiftY

  pure <<< fromJust $ (mkSafeCoordinate P2 $ C genX genY) >>= bToM (`inSubPlane` sp)

genFibreCoordinate :: Gen FibreCoordinate
genFibreCoordinate = do
  w64_1_x <- arbitrary
  w64_2_x <- arbitrary
  w64_3_x <- arbitrary
  w64_4_x <- arbitrary

  let genx = B.toNatural $ B.Word256 w64_1_x w64_2_x w64_3_x w64_4_x

  w64_1_y <- arbitrary
  w64_2_y <- arbitrary
  w64_3_y <- arbitrary
  w64_4_y <- arbitrary

  let geny = B.toNatural $ B.Word256 w64_1_y w64_2_y w64_3_y w64_4_y
  pure <<< fromJust $ mkSafeCoordinate fibrePlane $ C genx geny
