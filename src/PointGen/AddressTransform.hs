{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module PointGen.AddressTransform where

import PointGen.Address
import PointGen.Coordinate
import PointGen.Bip32
import PointGen.Plane
import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Data.ByteArray                as BA
import Data.Maybe
import Import hiding (id)

type MaxHashSize = 2^256
type FibrePlane = Plane2 MaxHashSize MaxHashSize
type FibreCoordinate = Coordinate2 MaxHashSize MaxHashSize
type FibreLocale a = Locale MaxHashSize MaxHashSize a

addressTransform :: Address a -> FibreCoordinate
addressTransform = textProject <<< pack <<< show

textProject :: Text -> FibreCoordinate
textProject t = fromJust $ mkCoordinate xCoord yCoord P2
  where
    baseToHash = t <> t -- double the initial text
    xBase = pack "x" <> baseToHash
    yBase = pack "y" <> baseToHash

    toCoordinate = word8sToInteger <<< BA.unpack <<< Crypto.hash256 <<< encodeUtf8

    xCoord = toCoordinate xBase
    yCoord = toCoordinate yBase

word8sToInteger :: [Word8] -> Integer
word8sToInteger [] = 0
word8sToInteger (a:as) = fromIntegral a + 256 * word8sToInteger as

data Locale (m :: Nat) (n :: Nat) (a :: Asset) = Locale { lCoordinate :: Coordinate2 m n, lAddress :: Address a, lPath :: DerivationPath } deriving Show
scaleLocale :: (KnownNats m1 n1, KnownNats m2 n2) => Plane2 m2 n2 -> Locale m1 n1 a -> Locale m2 n2 a
scaleLocale p2 locale = locale{ lCoordinate = scaledCoordinate }
  where
    scaledCoordinate = scaleToNewPlane p2 $ lCoordinate locale

deriveFibreLocale :: SAsset a -> XPub -> DerivationPath -> Maybe (FibreLocale a)
deriveFibreLocale sAsset xpub dpath = do
  address <- deriveAddress sAsset xpub dpath
  let coordinate = addressTransform address
  pure $ Locale coordinate address dpath

deriveLocale :: KnownNats m n => SAsset a -> XPub -> Plane2 m n -> DerivationPath -> Maybe (Locale m n a)
deriveLocale sAsset xpub p2 dpath = scaleLocale p2 <$> deriveFibreLocale sAsset xpub dpath

maxTries :: (KnownNat m, KnownNat n) => Plane2 m n -> Integer
maxTries p2 = 100 * xDim * yDim
  where
    (xDim, yDim) = plane2Dim p2

coordinateHunt :: KnownNats m n => SAsset a -> XPub -> Coordinate2 m n -> Maybe (Locale m n a)
coordinateHunt asset xpub target = find isTarget derivations
  where
    targetPlane = plane target
    tries = [0 .. maxTries (targetPlane)]
    tryPaths = fmap (mkPath <<< (0:) <<< pure) tries
    derivations = mapMaybe (deriveLocale asset xpub targetPlane) tryPaths -- [ (DerivationPath, Maybe ( Coordinate2 m n, Address a ) ) ]
    isTarget = (== target) <<< lCoordinate
