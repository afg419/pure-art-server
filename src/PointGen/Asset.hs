{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}

module PointGen.Asset where

import PointGen.Plane
import Data.Singletons.TH
import Database.Persist.Sql
import Import
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

class PlaneLike s => HasContext s where
  getAsset :: s -> Asset
data SContext (a :: Asset) (m :: Nat) (n :: Nat) where
  SContext :: (KnownNat m, KnownNat n) =>
    { sAsset :: SAsset a
    , dim :: Plane m n
    } -> SContext a m n

instance PlaneLike (SContext a m n) where
  getXDim = getXDim <<< dim
  getYDim = getYDim <<< dim

instance HasContext (SContext a m n) where
  getAsset = fromSing <<< sAsset
