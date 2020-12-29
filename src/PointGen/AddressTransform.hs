{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Import
import Data.Proxy

hotLocale :: SContext a m n -> XPub -> SLocale a m n
hotLocale cty xpub = deriveLocale cty xpub hotPath $>> fromJust

--------------------------------------------------------------------------------
-- FibrePlane represents the abstract space of all addresses understood geometrically
--------------------------------------------------------------------------------

type FibrePlane = Plane MaxHashSize MaxHashSize
type FibreCoordinate = SCoordinate MaxHashSize MaxHashSize
type MaxHashSize = 2^256

maxHashSize :: Natural
maxHashSize = fromIntegral <<< natVal <<$ Proxy @MaxHashSize

fibrePlane :: FibrePlane
fibrePlane = P2

addressTransform :: Address a -> FibreCoordinate
addressTransform = textProject <<< pack <<< show

textProject :: Text -> FibreCoordinate
textProject t = fromJust $ mkSafeCoordinate P2 $ C xCoord yCoord
  where
    baseToHash = t <> t -- double the initial text
    xBase = pack "x" <> baseToHash
    yBase = pack "y" <> baseToHash

    xCoord = hashToNatural xBase
    yCoord = hashToNatural yBase

words8sToNatural :: [Word8] -> Natural
words8sToNatural [] = 0
words8sToNatural (a:as) = fromIntegral a + 256 * words8sToNatural as

hashToNatural :: Text -> Natural
hashToNatural = words8sToNatural <<< BA.unpack <<< Crypto.hash256 <<< encodeUtf8

--------------------------------------------------------------------------------
-- SLocale's are geometric points with metadata about their derivation
--------------------------------------------------------------------------------

data SLocale (a :: Asset) (m :: Nat) (n :: Nat) = SLocale
  { lCoordinate :: SCoordinate m n
  , lAddress :: Address a
  , lPath :: DerivationPath
  } deriving (Show, Eq)

instance ToJSON (SLocale a m n) where
  toJSON SLocale{..} = object
    [ "coordinate" .= [getX lCoordinate, getY lCoordinate]
    , "address" .= tshow lAddress
    , "path" .= tshow lPath
    ]

data Locale = forall a m n. Locale (SLocale a m n)

instance CoordinateLike (SLocale a m n) where
  getX = getX <<< lCoordinate
  getY = getY <<< lCoordinate

deriveLocale :: SContext a m n -> XPub -> DerivationPath -> Maybe (SLocale a m n)
deriveLocale cty xpub dpath = scaleLocale (dim cty) <$> deriveFibreLocale (sAsset cty) xpub dpath

deriveSimple :: HasContext c => c -> XPub -> DerivationPath 

type FibreLocale a = SLocale a MaxHashSize MaxHashSize

deriveFibreLocale :: SAsset a -> XPub -> DerivationPath -> Maybe (FibreLocale a)
deriveFibreLocale sa xpub dpath = do
  address <- deriveAddress sa xpub dpath
  let coordinate = addressTransform address
  pure $ SLocale coordinate address dpath

scaleLocale :: Plane m2 n2 -> SLocale a m1 n1 -> SLocale a m2 n2
scaleLocale p2 locale = locale{ lCoordinate = scaledCoordinate }
  where
    scaledCoordinate = projectTo p2 $ lCoordinate locale
