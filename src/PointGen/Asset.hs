{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module PointGen.Asset where

import PointGen.Plane
import PointGen.Coordinate
import Data.Singletons.TH
import Database.Persist.Sql
import Import
import GHC.TypeNats
import Data.Aeson

$(singletons [d|
  data Asset = DOGE
    deriving (Show, Eq)
  |])

someAsset :: Asset -> SomeSing Asset
someAsset DOGE = SomeSing SDOGE

instance Enum Asset where
  fromEnum DOGE = 0
  toEnum 0 = DOGE
  toEnum _ = DOGE

instance Bounded Asset where
  minBound = DOGE
  maxBound = DOGE

instance ToJSON Asset where
    toJSON DOGE = String "DOGE"

instance FromJSON Asset where
  parseJSON = withText "asset" $ \t -> do

    if t == "DOGE"
      then pure DOGE
      else fail (unpack t)
parseAsset :: Text -> Maybe Asset
parseAsset = headMay <<< preimage' (pack <<< show)

instance PersistField Asset where
  toPersistValue = PersistText <<< pack <<< show
  fromPersistValue (PersistText t) = mToE "Invalid Asset" <<< parseAsset $ t
  fromPersistValue _ = Left "Expected Asset saved as text"
instance PersistFieldSql Asset where
  sqlType _ = SqlString

class TwoDimensional s => HasContext s where
  getAsset :: s -> Asset
data SContext (a :: Asset) (m :: Nat) (n :: Nat) where
  SContext :: (KnownNat m, KnownNat n) =>
    { sAsset :: SAsset a
    , dim :: Plane m n
    } -> SContext a m n

instance TwoDimensional (SContext a m n) where
  getX = getX <<< dim
  getY = getY <<< dim

instance HasContext (SContext a m n) where
  getAsset = fromSing <<< sAsset

withContext :: HasContext c => c -> (forall (a :: Asset) m n. (SContext a m n, ValidForContext a m n c) -> r) -> r
withContext c f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f (SContext s (P2 @m @n), toSafeCTY c)
  where
    a = getAsset c
    i = getX c
    j = getY c

newtype ValidForContext (a :: Asset) (m :: Nat) (n :: Nat) (s :: *) = ValidForContext (ValidForPlane m n s) deriving (Functor)

instance TwoDimensional s => TwoDimensional (ValidForContext a m n s) where
  getX = getX <<< fromSafeCTY
  getY = getY <<< fromSafeCTY

instance HasContext s => HasContext (ValidForContext a m n s) where
  getAsset = getAsset <<< fromSafeCTY

toSafeCTY :: (KnownNat m, KnownNat n) => s -> ValidForContext a m n s
toSafeCTY = ValidForPlane >>> ValidForContext

fromSafeCTY :: ValidForContext a m n s -> s
fromSafeCTY (ValidForContext (ValidForPlane s)) = s

mkSafeCTY :: HasContext s => SContext a m n -> s -> Maybe (ValidForContext a m n s)
mkSafeCTY scty s = mkSafeCoordinate (dim scty) s >>=
    \sD2 -> if getAsset scty == getAsset s
      then Just (ValidForContext sD2)
      else Nothing
