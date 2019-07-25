{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module PointGen.Bip32 where

import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Crypto.HDTree.Address as Crypto
import PointGen.Address
import PointGen.Asset
import Import hiding (get, id)
import Data.Aeson
import Database.Persist.Sql

deriveAddress :: SAsset a -> XPub -> DerivationPath -> Maybe (Address a)
deriveAddress SDOGE xpub dpath =
  fmap
  (DogeA <<< Crypto.getDogeP2PKHAddress Crypto.MainNet <<< Crypto._extKey <<< runXPub)
  $ deriveXPub xpub dpath

--------------------------------------------------------------------------------
-- XPubs
--------------------------------------------------------------------------------

newtype XPub = XPub {runXPub :: Crypto.XPub}
deriving instance Show XPub
deriving instance Eq XPub

instance FromJSON XPub where
  parseJSON = withText "Xpub String" $ \str -> do
    let mXPub = parseXPub <<< encodeUtf8 $ str
    case mXPub of
      Nothing -> fail $ "invalid xpub: " <> unpack str
      Just x -> pure x

instance PersistField XPub where
  toPersistValue = PersistText <<< decodeUtf8 <<< Crypto.toXAddress <<< runXPub
  fromPersistValue (PersistText t) = maybe (Left "Invalid XPub") Right <<< parseXPub <<< encodeUtf8 $ t
  fromPersistValue m = Left $ pack $ "Expected XPub saved as text " <> show m
instance PersistFieldSql XPub where
  sqlType _ = SqlString

parseXPub :: ByteString -> Maybe XPub
parseXPub = fmap XPub <<< Crypto.fromXAddress

--------------------------------------------------------------------------------
-- Derivation Paths
--------------------------------------------------------------------------------

newtype DerivationPath = DerivationPath Crypto.Path deriving Eq
instance Show DerivationPath where
  show (DerivationPath d) = show d

instance PersistField DerivationPath where
  toPersistValue = PersistText <<< pack <<< show
  fromPersistValue (PersistText dbPath) = dbPath
    $>> unpack
    >>> Crypto.parsePath
    >>> mToE "Text unable to be parsed as path"
    >>> fmap DerivationPath
  fromPersistValue dbValue = Left <<< pack $ "Expected DerivationPath saved as text, got: " <> show dbValue
instance PersistFieldSql DerivationPath where
  sqlType _ = SqlString

mkPath :: [Integer] -> DerivationPath
mkPath is = DerivationPath <<< Crypto.Path <<< fmap (Crypto.Index <<< fromIntegral) $ is

pathsForIndices :: [Integer] -> [DerivationPath]
pathsForIndices = fmap (mkPath <<< (0:) <<< pure)

deriveXPub :: XPub -> DerivationPath -> Maybe XPub
deriveXPub (XPub xpub') (DerivationPath path') = XPub <$> Crypto.derivePathPub xpub' path'
