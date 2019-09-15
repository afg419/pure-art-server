{-# LANGUAGE FlexibleInstances #-}

module PointGen.Address where

import qualified Crypto.HDTree.Address as Crypto
import PointGen.Asset

data Address (a :: Asset) where
  DogeA :: Crypto.DogeAddr -> Address 'DOGE
instance Show (Address a) where
  show (DogeA s) = show s
deriving instance Eq (Address a)

data SomeAddress where
  AddressExists :: Address a -> SomeAddress
instance Show SomeAddress where
  show (AddressExists a) = show a
