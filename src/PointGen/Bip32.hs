{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module PointGen.Bip32 where

import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Crypto.HDTree.Address as Crypto
import PointGen.Address
import Import hiding (get)
import Data.Aeson
import Database.Persist.Sql

newtype XPub = XPub {runXPub :: Crypto.XPub}

instance PersistField XPub where
  toPersistValue = PersistText <<< pack <<< show
  fromPersistValue (PersistText t) = maybe (Left "Invalid XPub") Right <<< parseXPub <<< encodeUtf8 $ t
  fromPersistValue _ = Left $ "Expected XPub saved as text"
instance PersistFieldSql XPub where
  sqlType _ = SqlString
deriving instance Show XPub
deriving instance Eq XPub
deriving instance Generic XPub
instance FromJSON XPub where
  parseJSON = withText "Xpub String" $ \str -> do
    let mXPub = parseXPub <<< encodeUtf8 $ str
    case mXPub of
      Nothing -> fail $ "invalid xpub: " <> unpack str
      Just x -> pure x

parseXPub :: ByteString -> Maybe XPub
parseXPub = fmap XPub <<< Crypto.fromXAddress

newtype DerivationPath = DerivationPath Crypto.Path deriving Eq
instance Show DerivationPath where
  show (DerivationPath d) = show d

instance PersistField DerivationPath where
  toPersistValue = PersistList <<< fmap (PersistInt64 <<< fromIntegral) <<< toIntegerPath
  fromPersistValue (PersistList int64s) = fmap mkPath <<< traverse extractInteger $ int64s
    where
      extractInteger :: PersistValue -> Either Text Integer
      extractInteger (PersistInt64 i) = Right <<< fromIntegral $ i
      extractInteger _ = Left $ "Expected DerivationPath index saved as integer"
  fromPersistValue _ = Left $ "Expected DerivationPath saved as list of integers"
instance PersistFieldSql DerivationPath where
  sqlType _ = SqlBlob

mkPath :: [Integer] -> DerivationPath
mkPath is = DerivationPath <<< Crypto.Path <<< fmap (Crypto.Index <<< fromIntegral) $ is

toIntegerPath :: DerivationPath -> [Integer]
toIntegerPath (DerivationPath (Crypto.Path is)) = fmap extractInteger is
  where
    extractInteger (Crypto.Index i) = fromIntegral i

deriveAddress :: SAsset a -> XPub -> DerivationPath -> Maybe (Address a)
deriveAddress SDOGE xpub dpath =
  fmap
  (DogeA <<< Crypto.getDogeP2PKHAddress Crypto.MainNet <<< Crypto._extKey <<< runXPub)
  $ deriveXPub xpub dpath

deriveXPub :: XPub -> DerivationPath -> Maybe XPub
deriveXPub (XPub xpub') (DerivationPath path') = XPub <$> Crypto.derivePathPub xpub' path'
