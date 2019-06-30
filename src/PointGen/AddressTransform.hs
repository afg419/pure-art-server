{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module PointGen.AddressTransform where

import PointGen.Address
import PointGen.Coordinate
import PointGen.Canvas
import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Data.ByteArray                as BA
import Data.Maybe
import Import

data Hash256 = Hash256
data Hash160 = Hash160

addressTransform :: (Show (Address a), TextTransform c) => c -> Address a -> Coordinate (MaxSize c) (MaxSize c)
addressTransform c = textTransform c <<< pack <<< show

class TextTransform c where
  type MaxSize c :: Nat
  textTransform :: c -> Text -> Coordinate (MaxSize c) (MaxSize c)

instance TextTransform Hash256 where
  type MaxSize Hash256 = 2^256
  textTransform _ t = fromJust $ mkCoordinate xCoord yCoord C2
    where
      baseToHash = t <> t -- double the initial text
      xBase = pack "x" <> baseToHash
      yBase = pack "y" <> baseToHash

      xHash = Crypto.hash256 <<< encodeUtf8 $ xBase
      yHash = Crypto.hash256 <<< encodeUtf8 $ yBase

      xCoord = word8sToInteger <<< BA.unpack $ xHash
      yCoord = word8sToInteger <<< BA.unpack $ yHash

word8sToInteger :: [Word8] -> Integer
word8sToInteger [] = 0
word8sToInteger (a:as) = fromIntegral a + 256 * word8sToInteger as
