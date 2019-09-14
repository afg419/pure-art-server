module PointGen.Plane where

import Import
import Data.Proxy
import GHC.TypeNats

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

leqDimensionsThan :: Plane2 m1 n1 -> Plane2 m2 n2 -> Bool
leqDimensionsThan p1 p2 = p1x <= p2x && p1y <= p2y
  where
    (p1x, p1y) = dimensions p1
    (p2x, p2y) = dimensions p2

withPlaneTy :: (Natural, Natural) -> (forall m n. Plane2 m n -> r) -> r
withPlaneTy (i, j) f = case (someNatVal i, someNatVal j) of
  (SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ P2 @m @n
