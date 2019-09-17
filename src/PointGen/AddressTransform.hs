{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
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

hotLocale :: SCTY a m n -> XPub -> SLocale a m n
hotLocale cty xpub = deriveLocale cty xpub hotPath $>> fromJust

--------------------------------------------------------------------------------
-- FibrePlane represents the abstract space of all addresses understood geometrically
--------------------------------------------------------------------------------

type FibrePlane = Plane2 MaxHashSize MaxHashSize
type FibreCoordinate = SCoordinate2 MaxHashSize MaxHashSize
type MaxHashSize = 2^256

maxHashSize :: Natural
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
  { lCoordinate :: SCoordinate2 m n
  , lAddress :: Address a
  } deriving (Show, Eq)

instance ToJSON (SLocale a m n) where
  toJSON SLocale{..} = object
    [ "coordinate" .= array [cx lCoordinate, cy lCoordinate]
    , "address" .= (String $ tshow lAddress)
    ]

data Locale = forall a m n. Locale (SLocale a m n)

instance TwoDimensional (SLocale a m n) where
  getX = getX <<< lCoordinate
  getY = getY <<< lCoordinate

deriveLocale :: SCTY a m n -> XPub -> DerivationPath -> Maybe (SLocale a m n)
deriveLocale cty xpub dpath = scaleLocale (dim cty) <$> deriveFibreLocale (sAsset cty) xpub dpath

mkLocale :: SCTY a m n -> PublicKey -> SLocale a m n
mkLocale scty pk = scaleLocale (dim scty) <<$ mkFibreLocale (sAsset scty) pk

type FibreLocale a = SLocale a MaxHashSize MaxHashSize

deriveFibreLocale :: SAsset a -> XPub -> DerivationPath -> Maybe (FibreLocale a)
deriveFibreLocale sa xpub dpath = do
  address <- deriveAddress sa xpub dpath
  let coordinate = addressTransform address
  pure $ SLocale coordinate address

mkFibreLocale :: SAsset a -> PublicKey -> FibreLocale a
mkFibreLocale sa pk = SLocale coord addr
  where
    addr = mkAddress sa pk
    coord = addressTransform addr

scaleLocale :: Plane2 m2 n2 -> SLocale a m1 n1 -> SLocale a m2 n2
scaleLocale p2 locale = locale{ lCoordinate = scaledCoordinate }
  where
    scaledCoordinate = projectTo p2 $ lCoordinate locale
