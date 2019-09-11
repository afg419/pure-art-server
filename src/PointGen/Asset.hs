{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}

module PointGen.Asset where

import Data.Singletons.TH
import Database.Persist.Sql
import Import

$(singletons [d|
  data Asset = DOGE
    deriving (Show, Eq, Generic)
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

instance ToJSON Asset
instance FromJSON Asset

parseAsset :: Text -> Maybe Asset
parseAsset = headMay <<< preimage' (pack <<< show)

instance PersistField Asset where
  toPersistValue = PersistText <<< pack <<< show
  fromPersistValue (PersistText t) = mToE "Invalid Asset" <<< parseAsset $ t
  fromPersistValue _ = Left $ "Expected Asset saved as text"
instance PersistFieldSql Asset where
  sqlType _ = SqlString
