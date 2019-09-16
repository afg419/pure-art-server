{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module PointGen.Bip32 where

import qualified Crypto.HDTree.Bip32 as Crypto
import qualified Crypto.HDTree.Address as Crypto
import PointGen.Address
import PointGen.Asset
import Import hiding (get)
import Data.Aeson
import Database.Persist.Sql
import Data.Maybe (fromJust)
import qualified Data.Serialize as S

deriveAddress :: SAsset a -> XPub -> DerivationPath -> Maybe (Address a)
deriveAddress sa xpub dpath = fmap (mkAddress sa) mpub
  where
    mpub = fmap xpubToPub <<$ deriveXPub xpub dpath

mkPublicKey :: XPub -> DerivationPath -> Maybe PublicKey
mkPublicKey xpub dpath = fmap xpubToPub <<$ deriveXPub xpub dpath

mkAddress :: SAsset a -> PublicKey -> Address a
mkAddress SDOGE pk =
  (DogeA <<< Crypto.getDogeP2PKHAddress Crypto.MainNet <<< runPubKey)
  $ pk

hotAddress :: SAsset a -> XPub -> Address a
hotAddress sa x = deriveAddress sa x hotPath $>> fromJust

--------------------------------------------------------------------------------
-- XPubs
--------------------------------------------------------------------------------

newtype XPub = XPub {runXPub :: Crypto.XPub}

instance Read XPub where
  readsPrec _ _ = debug "Fix this" Import.undefined
instance PathPiece XPub where
  toPathPiece = tshow
  fromPathPiece str = parseXPub <<< encodeUtf8 $ str

newtype PublicKey = PublicKey {runPubKey :: Crypto.PublicKey} deriving (Eq)
instance Show PublicKey where
  show = show <<< runPubKey
instance PersistField PublicKey where
  toPersistValue = PersistText <<< decodeUtf8 <<< Crypto.getCompressed <<< runPubKey
  fromPersistValue (PersistText t) = maybe (Left "Invalid PublicKey") Right <<< parsePublicKey <<< encodeUtf8 $ t
  fromPersistValue m = Left $ pack $ "Expected PublicKey saved as text " <> show m
instance PersistFieldSql PublicKey where
  sqlType _ = SqlString

parsePublicKey :: ByteString -> Maybe PublicKey
parsePublicKey = fmap PublicKey <<< eToM <<< S.decode

xpubToPub :: XPub -> PublicKey
xpubToPub = runXPub >>> Crypto._extKey >>> PublicKey

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

newtype DerivationPath = DerivationPath Crypto.Path deriving (Eq)

instance ToJSON DerivationPath where
  toJSON p = String $ tshow p

instance FromJSON DerivationPath where
  parseJSON = withText "derivation path" $ \text -> pure $ read (unpack $ debug "the fuck" text)

instance Ord DerivationPath where
  p1 <= p2 = unPath p1 <= unPath p2

instance Read DerivationPath where
  readsPrec _ ('m':'/':s) = debug "uh is this right?" (fmap (mkPath *** id) ((readsPrec 0 s) :: [([Natural], String)]))
  readsPrec _ _ = []

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

mkPath :: [Natural] -> DerivationPath
mkPath is = DerivationPath <<< Crypto.Path <<< fmap (Crypto.Index <<< fromIntegral) $ is

unPath :: DerivationPath -> [Natural]
unPath (DerivationPath (Crypto.Path p)) = fmap (fromIntegral <<< Crypto.getIndex) p

pathsForIndices :: [Natural] -> [DerivationPath]
pathsForIndices = fmap (mkPath <<< (0:) <<< pure)

deriveXPub :: XPub -> DerivationPath -> Maybe XPub
deriveXPub (XPub xpub') (DerivationPath path') = XPub <$> Crypto.derivePathPub xpub' path'

hotPath :: DerivationPath
hotPath = mkPath [1,0]
