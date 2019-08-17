{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Effects.CanvasGeneration where

import PointGen
import Model
import Effects.Common
import Effects.Interpreters
import Database.Persist
import Data.Singletons
import Data.Maybe (fromJust)
import Import hiding (id, undefined)

originAddressPath :: DerivationPath
originAddressPath = mkPath [0,0]

class Effect s => CanvasGeneration s where
  -- returns id of record, and 0/0 address
  insertCanvas2 :: KnownNats m n => XPub -> Plane2 m n -> s (Either String (SCanvas2Id m n))
  getCanvas2    :: KnownNats m n => SCanvas2Id m n -> s (Maybe (SCanvas2 m n))
  updateCanvas2NextPathIndex :: SCanvas2Id m n -> Integer -> s ()

  -- this uses a repsertMany so no duplicate coordinates stored
  insertPlane2Locales :: KnownNats m n => SAsset a -> SCanvas2Id m n -> [Locale a m n] -> s ()
  getPlane2Locales :: KnownNats m n => SAsset a -> SCanvas2Id m n -> s [Locale a m n]

instance CanvasGeneration PsqlDB where
  insertCanvas2 xpub p2 = do
    now <- liftIO getCurrentTime
    canvas2Id <- PsqlDB <<< insert <<$ canvas now
    pure <<< Right <<< SCanvas2Id <<$ canvas2Id
      where
        (xSize, ySize) = plane2Dim p2
        canvas = \now -> Canvas2
          xpub
          (fromIntegral xSize)
          (fromIntegral ySize)
          0
          now
          now
  getCanvas2 (SCanvas2Id cId) = do
    mCanvas2 <- PsqlDB <<< get $ cId
    pure <<< fmap SCanvas2 $ mCanvas2
  updateCanvas2NextPathIndex (SCanvas2Id cId) nextIndex = [Canvas2NextPathIndex =. (fromIntegral nextIndex)]
    $>> update cId
    >>> PsqlDB
  insertPlane2Locales sAsset scId locales = do
    now <- liftIO getCurrentTime
    PsqlDB <<< repsertMany $ fmap ( toLocaleRecordKey scId &&& (withSingI sAsset) (toLocaleRecord scId now)) locales
  getPlane2Locales sAsset scid@(SCanvas2Id cid) =
    selectList [ LocaleRecordCanvas2Id ==. cid, LocaleRecordAsset ==. (fromSing sAsset)] []
    $>> fmap ((fmap $ entityVal >>> fromLocaleRecord sAsset scid) >>> catMaybes)
    >>> PsqlDB
