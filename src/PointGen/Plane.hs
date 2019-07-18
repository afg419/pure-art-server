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

data Plane2 (m :: Nat) (n :: Nat) where
  P2 :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n
deriving instance Show (Plane2 m n)
deriving instance Eq (Plane2 m n)

plane2Dim :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n -> (Integer, Integer)
plane2Dim _ = (fromIntegral <<< natVal $ Proxy @m , fromIntegral <<< natVal $ Proxy @n)

data PlaneStock = SmallStock | MediumStock | LargeStock | XLargeStock
  deriving (Eq, Show, Generic)
instance FromJSON PlaneStock

smallPlane :: Plane2 10 10
smallPlane = P2
mediumPlane :: Plane2 10 10
mediumPlane = P2
largePlane :: Plane2 10 10
largePlane = P2
xLargePlane :: Plane2 10 10
xLargePlane = P2

withPlaneStock :: PlaneStock -> ( forall m n. (KnownNat m, KnownNat n) => Plane2 m n -> s ) -> s
withPlaneStock SmallStock wPlane = wPlane smallPlane
withPlaneStock MediumStock wPlane = wPlane mediumPlane
withPlaneStock LargeStock wPlane = wPlane largePlane
withPlaneStock XLargeStock wPlane = wPlane xLargePlane
