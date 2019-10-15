{-# LANGUAGE FlexibleInstances #-}

module PointGen.Address where

import qualified Crypto.HDTree.Address as Crypto
import PointGen.Asset
import Import

data Address (a :: Asset) where
  DogeA :: Crypto.DogeAddr -> Address 'DOGE
instance Show (Address a) where
  show (DogeA s) = show s
deriving instance Eq (Address a)

parseAddress :: SAsset a -> Text -> Maybe (Address a)
parseAddress SDOGE t =
  if Crypto.isValidDogeAddr addr
    then Just <<$ DogeA addr
    else Nothing
  where
    addr = Crypto.DogeAddr <<$ encodeUtf8 t


data SomeAddress where
  AddressExists :: Address a -> SomeAddress
instance Show SomeAddress where
  show (AddressExists a) = show a
