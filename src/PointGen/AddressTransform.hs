{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PointGen.AddressTransform where

import PointGen.Address
import PointGen.Coordinate
import PointGen.Bip32
import PointGen.Plane
import PointGen.PlaneTransform
import PointGen.Asset
import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Data.ByteArray                as BA
import Data.Maybe
import Import hiding (Proxy)
import Data.Proxy

hotLocale :: KnownNats m n => SAsset a -> XPub -> Plane2 m n -> SLocale a m n
hotLocale s xpub p = deriveLocale s xpub p hotPath $>> fromJust

--------------------------------------------------------------------------------
-- FibrePlane represents the abstract space of all addresses understood geometrically
--------------------------------------------------------------------------------

type FibrePlane = Plane2 MaxHashSize MaxHashSize
type FibreCoordinate = SCoordinate2 MaxHashSize MaxHashSize
type MaxHashSize = 2^256

maxHashSize :: Integer
maxHashSize = fromIntegral <<< natVal <<$ Proxy @MaxHashSize

fibrePlane :: FibrePlane
fibrePlane = P2

addressTransform :: Address a -> FibreCoordinate
addressTransform = textProject <<< pack <<< show

textProject :: Text -> FibreCoordinate
textProject t = fromJust $ mkCoordinate xCoord yCoord P2
  where
    baseToHash = t <> t -- double the initial text
    xBase = pack "x" <> baseToHash
    yBase = pack "y" <> baseToHash

    xCoord = hashIntoInteger xBase
    yCoord = hashIntoInteger yBase

word8sToInteger :: [Word8] -> Integer
word8sToInteger [] = 0
word8sToInteger (a:as) = fromIntegral a + 256 * word8sToInteger as

hashIntoInteger :: Text -> Integer
hashIntoInteger = word8sToInteger <<< BA.unpack <<< Crypto.hash256 <<< encodeUtf8

--------------------------------------------------------------------------------
-- SLocale's are geometric points with metadat about their derivation
--------------------------------------------------------------------------------

data SLocale (a :: Asset) (m :: Nat) (n :: Nat) = SLocale
  { lCoordinate :: SCoordinate2 m n
  , lAddress :: Address a
  , lPath :: DerivationPath
  } deriving (Show, Eq)

data Locale = forall a m n. Locale (SLocale a m n)

deriveLocale :: KnownNats m n => SAsset a -> XPub -> Plane2 m n -> DerivationPath -> Maybe (SLocale a m n)
deriveLocale sAsset xpub p2 dpath = scaleLocale p2 <$> deriveFibreLocale sAsset xpub dpath

type FibreLocale a = SLocale a MaxHashSize MaxHashSize

deriveFibreLocale :: SAsset a -> XPub -> DerivationPath -> Maybe (FibreLocale a)
deriveFibreLocale sAsset xpub dpath = do
  address <- deriveAddress sAsset xpub dpath
  let coordinate = addressTransform address
  pure $ SLocale coordinate address dpath

scaleLocale :: (KnownNats m1 n1, KnownNats m2 n2) => Plane2 m2 n2 -> SLocale a m1 n1 -> SLocale a m2 n2
scaleLocale p2 locale = locale{ lCoordinate = scaledCoordinate }
  where
    scaledCoordinate = projectTo p2 $ lCoordinate locale
