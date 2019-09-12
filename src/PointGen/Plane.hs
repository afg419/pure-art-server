{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}

module PointGen.Plane where

import Import
import Data.Proxy


dimensions :: forall m n. Plane2 m n -> (Natural, Natural)
dimensions P2 = (fromIntegral <<< natVal $ Proxy @m, fromIntegral <<< natVal $ Proxy @n)

dimensions' :: forall m n s. (KnownNat m, KnownNat n) => s m n -> (Natural, Natural)
dimensions' _ = (fromIntegral <<< natVal $ Proxy @m, fromIntegral <<< natVal $ Proxy @n)

data Plane2 (m :: Nat) (n :: Nat) where
  P2 :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n
deriving instance Show (Plane2 m n)
deriving instance Eq (Plane2 m n)

plane2For ::  forall m n s. (KnownNat m, KnownNat n) => s m n -> Plane2 m n
plane2For _ = P2

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

leqDimensionsThan :: Plane2 m1 n1 -> Plane2 m2 n2 -> Bool
leqDimensionsThan p1 p2 = p1x <= p2x && p1y <= p2y
  where
    (p1x, p1y) = dimensions p1
    (p2x, p2y) = dimensions p2
