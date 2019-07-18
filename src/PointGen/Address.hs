{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}


module PointGen.Address where

import qualified Crypto.HDTree.Address as Crypto
import Data.Singletons.TH
import Database.Persist.Sql
import Import

$(singletons [d|
  data Asset = DOGE
    deriving (Show, Eq, Generic)
  |])

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

data Address (a :: Asset) where
  DogeA :: Crypto.DogeAddr -> Address 'DOGE
instance Show (Address a) where
  show (DogeA s) = show s
deriving instance Eq (Address 'DOGE)

data SomeAddress where
  AddressExists :: Address a -> SomeAddress
instance Show SomeAddress where
  show (AddressExists a) = show a
