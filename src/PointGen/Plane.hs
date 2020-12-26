{-# LANGUAGE DeriveFunctor #-}
module PointGen.Plane where

import Import
import Data.Proxy
import GHC.TypeNats

class TwoDimensional c where
  getX :: c -> Natural
  getY :: c -> Natural

instance TwoDimensional (Plane m n) where
  getX = fst <<< dimensions
  getY = snd <<< dimensions

dimensions :: forall m n. Plane m n -> (Natural, Natural)
dimensions P2 = (fromIntegral <<< natVal $ Proxy @m, fromIntegral <<< natVal $ Proxy @n)

data Plane (m :: Nat) (n :: Nat) where
  P2 :: forall m n. (KnownNat m, KnownNat n) => Plane m n
deriving instance Show (Plane m n)
deriving instance Eq (Plane m n)

leqDimensionsThan :: Plane m1 n1 -> Plane m2 n2 -> Bool
leqDimensionsThan p1 p2 = p1x <= p2x && p1y <= p2y
  where
    (p1x, p1y) = dimensions p1
    (p2x, p2y) = dimensions p2

withPlaneTy :: (Natural, Natural) -> (forall m n. Plane m n -> r) -> r
withPlaneTy (i, j) f = case (someNatVal i, someNatVal j) of
  (SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ P2 @m @n

data ValidForPlane (m :: Nat) (n :: Nat) (s :: *) where
  ValidForPlane :: forall m n s. (KnownNat m, KnownNat n) => s -> ValidForPlane m n s
deriving instance Functor (ValidForPlane m n)


planeDimensions :: forall m n s. ValidForPlane m n s -> (Natural, Natural)
planeDimensions (ValidForPlane _) = (fromIntegral <<< natVal $ Proxy @m, fromIntegral <<< natVal $ Proxy @n)

instance Show s => Show (ValidForPlane m n s) where
  show (ValidForPlane s) = show s
instance Eq s => Eq (ValidForPlane m n s) where
  ValidForPlane s == ValidForPlane t = s == t

instance Ord s => Ord (ValidForPlane m n s) where
  ValidForPlane s <= ValidForPlane t = s <= t

fromSafe2D :: ValidForPlane m n s -> s
fromSafe2D (ValidForPlane s) = s

-- Note: the x and y returned are the x and y of s, not m and n of the plane.
instance TwoDimensional s => TwoDimensional (ValidForPlane m n s) where
  getX = getX <<< fromSafe2D
  getY = getY <<< fromSafe2D

-- l1 distance between two dimensional coordinates
l1Dist :: (TwoDimensional c, TwoDimensional d) => c -> d -> Natural
l1Dist c d = fromIntegral <<$ abs (getXInt c - getXInt d) + abs (getYInt c - getYInt d)
  where
    getXInt :: TwoDimensional e => e -> Integer
    getXInt  = toInteger <<< getX
    getYInt :: TwoDimensional e => e -> Integer
    getYInt = toInteger <<< getY
