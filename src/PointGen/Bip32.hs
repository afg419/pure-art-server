{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module PointGen.Bip32 where

import           Data.Serialize
import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Crypto.HDTree.Address as Crypto
import PointGen.Address
import Import hiding (get)

newtype XPub = XPub {runXPub :: Crypto.XPub}


deriving instance Show XPub
deriving instance Eq XPub
deriving instance Generic XPub
instance Serialize XPub where
  get = XPub <$> get

newtype DerivationPath = DerivationPath Crypto.Path deriving (Eq, Show)

deriveAddress :: SAsset a -> XPub -> DerivationPath -> Maybe (Address a)
deriveAddress SDOGE xpub dpath = (DogeA <<< Crypto.getDogeP2PKHAddress Crypto.MainNet <<< Crypto._extKey <<< runXPub) <$> deriveXPub xpub dpath

deriveXPub :: XPub -> DerivationPath -> Maybe XPub
deriveXPub (XPub xpub') (DerivationPath path') = XPub <$> Crypto.derivePathPub xpub' path'
