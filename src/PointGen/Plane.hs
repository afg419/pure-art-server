{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module PointGen.Plane where

import Import hiding (Proxy)
import Data.Proxy

data Plane2 (m :: Nat) (n :: Nat) where
  P2 :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n

deriving instance Show (Plane2 m n)
deriving instance Eq (Plane2 m n)

plane2Dim :: forall m n. (KnownNat m, KnownNat n) => Plane2 m n -> (Integer, Integer)
plane2Dim _ = (fromIntegral <<< natVal $ Proxy @m , fromIntegral <<< natVal $ Proxy @n)
