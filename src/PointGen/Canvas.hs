{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module PointGen.Canvas where

import Import hiding (Proxy)
import Data.Proxy

fibrePlaneSize :: Integer
fibrePlaneSize = 2 ^ (160 :: Integer)

data Canvas2 (m :: Nat) (n :: Nat) where
  C2 :: forall m n. (KnownNat m, KnownNat n) => Canvas2 m n

deriving instance Show (Canvas2 m n)

canvas2Dim :: forall m n. (KnownNat m, KnownNat n) => Canvas2 m n -> (Integer, Integer)
canvas2Dim _ = (fromIntegral <<< natVal $ Proxy @m , fromIntegral <<< natVal $ Proxy @n)

fibrePlane :: Canvas2 (2^160) (2^160)
fibrePlane = C2
