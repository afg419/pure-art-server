{-# LANGUAGE OverloadedStrings #-}

module Effects.CanvasGeneration where

import PointGen
import Model
import Effects.Common
import Effects.Interpreters
import Database.Persist
import Data.Singletons
import Import hiding (undefined)

class Effect s => CanvasGeneration s where
  -- returns id of record, and 0/0 address
  insertPublicKeyGenerator :: XPub -> s PublicKeyGeneratorId
  getPublicKeyGenerator    :: PublicKeyGeneratorId -> s (Maybe PublicKeyGenerator)

  updateGeneratorIndex :: PublicKeyGeneratorId -> Natural -> s ()

  -- this uses a repsertMany so no duplicate coordinates stored
  insertPublicKeys :: PublicKeyGeneratorId -> [(PublicKey, DerivationPath)] -> s ()
  getPlane2Locales :: CTY a m n -> PublicKeyGeneratorId -> s [SLocale a m n]

instance CanvasGeneration PsqlDB where
  insertPublicKeyGenerator xpub = do
    now <- liftIO getCurrentTime
    PsqlDB <<< insert <<$ PublicKeyGenerator xpub 0 now now
  getPublicKeyGenerator pkgenId = PsqlDB <<< get $ pkgenId
  updateGeneratorIndex pkgenId nextIndex = [PublicKeyGeneratorNextPathIndex =. (fromIntegral nextIndex)]
    $>> update pkgenId
    >>> PsqlDB
  insertPublicKeys pkgenId pks = do
    now <- liftIO getCurrentTime
    let toPublicKeyRecord (pk, dp) = (PublicKeyRecordId pkgenId dp, PublicKeyRecord pkgenId pk dp now now)
    PsqlDB <<< repsertMany <<$ fmap toPublicKeyRecord pks
  getPlane2Locales cty pkgenId =
    selectList [ LocaleRecordCanvas2Id ==. cid] []
    $>> fmap (fmap $ entityVal >>> fromLocaleRecord cty)
    >>> PsqlDB
    >>> fmap catMaybes
