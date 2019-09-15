{-# LANGUAGE OverloadedStrings #-}

module Effects.CanvasGeneration where

import PointGen
import Model
import Effects.Common
import Effects.Interpreters
import Database.Persist
import Import hiding (undefined)

class Effect s => CanvasGeneration s where
  insertPublicKeyGenerator :: XPub -> s PublicKeyGeneratorId
  getPublicKeyGenerator    :: XPub -> s (Maybe (Entity PublicKeyGenerator))
  getPublicKeyGeneratorId    :: XPub -> s (Maybe PublicKeyGeneratorId)
  updateGeneratorIndex :: PublicKeyGeneratorId -> Natural -> s ()
  insertPublicKeys :: PublicKeyGeneratorId -> [(PublicKey, DerivationPath)] -> s ()
  getPublicKeys :: PublicKeyGeneratorId -> s [PublicKeyRecord]

instance CanvasGeneration PsqlDB where
  insertPublicKeyGenerator xpub = do
    now <- liftIO getCurrentTime
    PsqlDB <<< insert <<$ PublicKeyGenerator xpub 0 now now
  getPublicKeyGenerator xpub = selectFirst [ PublicKeyGeneratorXpub ==. xpub] []
    $>> PsqlDB
  getPublicKeyGeneratorId xpub = selectFirst [ PublicKeyGeneratorXpub ==. xpub] []
    $>> fmap (fmap entityKey)
    >>> PsqlDB
  updateGeneratorIndex pkgenId nextIndex = [PublicKeyGeneratorNextPathIndex =. (fromIntegral nextIndex)]
    $>> update pkgenId
    >>> PsqlDB
  insertPublicKeys pkgenId pks = do
    now <- liftIO getCurrentTime
    let toPublicKeyRecord (pk, dp) = (PublicKeyRecordKey pkgenId dp, PublicKeyRecord pkgenId pk dp now now)
    PsqlDB <<< repsertMany <<$ fmap toPublicKeyRecord pks
  getPublicKeys pkgenId =
    selectList [ PublicKeyRecordPublicKeyGeneratorId ==. pkgenId] []
    $>> fmap (fmap entityVal)
    >>> PsqlDB
