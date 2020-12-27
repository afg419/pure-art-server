{-# LANGUAGE DeriveFunctor #-}
module PointGen.Plane where

import Import
import Data.Proxy
import GHC.TypeNats
class PlaneLike c where
  getXDim :: c -> Natural
  getYDim :: c -> Natural

instance PlaneLike (Plane m n) where
  getXDim P2 = fromIntegral <<< natVal <<$ Proxy @m
  getYDim P2 = fromIntegral <<< natVal <<$ Proxy @n

data Plane (m :: Nat) (n :: Nat) where
  P2 :: forall m n. (KnownNat m, KnownNat n) => Plane m n
deriving instance Show (Plane m n)
deriving instance Eq (Plane m n)

dimensions :: PlaneLike s => s -> (Natural, Natural)
dimensions = getXDim &&& getYDim

leqDimensionsThan :: Plane m1 n1 -> Plane m2 n2 -> Bool
leqDimensionsThan p1 p2 = p1x <= p2x && p1y <= p2y
  where
    (p1x, p1y) = dimensions p1
    (p2x, p2y) = dimensions p2

withPlaneTy :: (Natural, Natural) -> (forall m n. Plane m n -> r) -> r
withPlaneTy (i, j) f = case (someNatVal i, someNatVal j) of
  (SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ P2 @m @n

-- we wrap a type s with ValidForPlane m n when s is coherent with the plane.
-- see mkSafeCoordinate for a safe constructor for coordinates
data ValidForPlane (m :: Nat) (n :: Nat) (s :: *) where
  ValidForPlane :: forall m n s. (KnownNat m, KnownNat n) => s -> ValidForPlane m n s
deriving instance Functor (ValidForPlane m n)


instance PlaneLike (ValidForPlane m n s) where
  getXDim (ValidForPlane _) = fromIntegral <<< natVal $ Proxy @m
  getYDim (ValidForPlane _) = fromIntegral <<< natVal $ Proxy @n

instance Show s => Show (ValidForPlane m n s) where
  show (ValidForPlane s) = show s
instance Eq s => Eq (ValidForPlane m n s) where
  ValidForPlane s == ValidForPlane t = s == t

instance Ord s => Ord (ValidForPlane m n s) where
  ValidForPlane s <= ValidForPlane t = s <= t

unwrapValid :: ValidForPlane m n s -> s
unwrapValid (ValidForPlane s) = s
