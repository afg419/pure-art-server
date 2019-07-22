{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.PointGen where

import Test.QuickCheck
import PointGen
import Import hiding (undefined)

instance Arbitrary PlaneStock where
  arbitrary = arbitraryBoundedEnum

withRandomPlane :: (forall m n. KnownNats m n => Plane2 m n -> s) -> Gen s
withRandomPlane s = do
  stock <- arbitrary
  pure $ withPlaneStock stock s

withRandomCoordinate :: (forall m n. KnownNats m n => Coordinate2 m n -> s) -> Gen s
withRandomCoordinate s = do
  stock <- arbitrary
  withPlaneStock stock $ genCoordinate >>> fmap s

genCoordinate :: KnownNats m n => Plane2 m n -> Gen (Coordinate2 m n)
genCoordinate targetPlane = fmap (projectTo targetPlane) $ genFibreCoordinate

genSubplaneCoordinate :: KnownNats m n => SubPlane2 m n -> Gen (Coordinate2 m n)
genSubplaneCoordinate sp = genCoordinate P2 `suchThat` (`inSubPlane` sp)

genFibreCoordinate :: Gen FibreCoordinate
genFibreCoordinate = do
  genX <- arbitrarySizedIntegral `suchThat` (\i -> 0 <= i && i <= maxHashSize)
  genY <- arbitrarySizedIntegral `suchThat` (\i -> 0 <= i && i <= maxHashSize)
  pure $ Coordinate2 genX genY fibrePlane
  -- Gen a -> (a -> Bool) -> Gen a Source#
