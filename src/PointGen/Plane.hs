{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module PointGen.Plane where

import Import hiding (Proxy)
import Data.Proxy
import Data.Singletons

data Plane2 (m :: Nat) (n :: Nat) where
  P2 :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n
deriving instance Show (Plane2 m n)
deriving instance Eq (Plane2 m n)

plane2Dim :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n -> (Integer, Integer)
plane2Dim _ = (fromIntegral <<< natVal $ Proxy @m , fromIntegral <<< natVal $ Proxy @n)

plane2Dim' :: forall m n s. (KnownNat m, KnownNat n) => s m n -> (Integer, Integer)
plane2Dim' _ = (fromIntegral <<< natVal $ Proxy @m , fromIntegral <<< natVal $ Proxy @n)

data PlaneStock = SmallStock | MediumStock | LargeStock | XLargeStock
  deriving (Eq, Show, Generic, Bounded, Enum)
instance FromJSON PlaneStock

smallPlane :: Plane2 10 10
smallPlane = P2
mediumPlane :: Plane2 100 100
mediumPlane = P2
largePlane :: Plane2 1000 1000
largePlane = P2
xLargePlane :: Plane2 10000 10000
xLargePlane = P2

withPlaneStock :: PlaneStock -> ( forall m n. (KnownNat m, KnownNat n) => Plane2 m n -> s ) -> s
withPlaneStock SmallStock wPlane = wPlane smallPlane
withPlaneStock MediumStock wPlane = wPlane mediumPlane
withPlaneStock LargeStock wPlane = wPlane largePlane
withPlaneStock XLargeStock wPlane = wPlane xLargePlane

withDimensions :: (Integer, Integer) -> ( forall m n. (KnownNat m, KnownNat n) => Plane2 m n -> s ) -> Maybe s
withDimensions (i,j) f = case (someNatVal i, someNatVal j) of
  (Just (SomeNat (Proxy :: Proxy m)), Just (SomeNat (Proxy :: Proxy n))) ->
    (P2 :: Plane2 m n) $>> f >>> Just
  _ -> Nothing

leqDimensionsThan :: (KnownNat m1, KnownNat n1, KnownNat m2, KnownNat n2)
  => Plane2 m1 n1 -> Plane2 m2 n2 -> Bool
leqDimensionsThan p1 p2 = p1x <= p2x && p1y <= p2y
  where
    (p1x, p1y) = plane2Dim p1
    (p2x, p2y) = plane2Dim p2
