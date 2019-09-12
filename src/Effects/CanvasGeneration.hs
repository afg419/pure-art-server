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
  insertCanvas2 :: CTY a m n -> XPub -> s (SCanvas2Id a m n)
  getCanvas2    :: Canvas2Id -> s (Maybe Canvas2)

  -- will return Left if Canvas2 does not conform to (CTY a m n) or if Id DNE
  sGetCanvas2    :: CTY a m n -> SCanvas2Id a m n -> s (Either Text (SCanvas2 a m n))
  updateCanvas2NextPathIndex :: Canvas2Id -> Integer -> s ()

  -- this uses a repsertMany so no duplicate coordinates stored
  insertPlane2Locales :: CTY a m n -> SCanvas2Id a m n -> [SLocale a m n] -> s ()
  getPlane2Locales :: CTY a m n -> SCanvas2Id a m n -> s [SLocale a m n]

instance CanvasGeneration PsqlDB where
  insertCanvas2 cty xpub = do
    now <- liftIO getCurrentTime
    cid <- PsqlDB <<< insert <<$ canvas now
    pure <<$ SCanvas2Id cid
      where
        (xSize, ySize) = dimensions (dim cty)
        canvas = \now -> Canvas2
          xpub
          (fromIntegral xSize)
          (fromIntegral ySize)
          (fromSing <<< sAsset <<$ cty)
          0
          now
          now
  getCanvas2 cId = PsqlDB <<< get $ cId
  sGetCanvas2 cty (SCanvas2Id cid) = getCanvas2 cid
    $>> fmap notFoundE
    >>> fmap (>>= mkSCanvas2 cty >>> dimMismatchE)
    where
      notFoundE = mToE "Not Found"
      dimMismatchE = mToE "Canvas dimension mismatch"
  updateCanvas2NextPathIndex cid nextIndex = [Canvas2NextPathIndex =. (fromIntegral nextIndex)]
    $>> update cid
    >>> PsqlDB
  insertPlane2Locales cty scid locales = do
    now <- liftIO getCurrentTime
    PsqlDB <<< repsertMany $ fmap (toLocaleRecordKey scid &&& (withSingI $ sAsset cty) (toLocaleRecord scid now)) locales
  getPlane2Locales cty (SCanvas2Id cid) =
    selectList [ LocaleRecordCanvas2Id ==. cid] []
    $>> fmap (fmap $ entityVal >>> fromLocaleRecord cty)
    >>> PsqlDB
    >>> fmap catMaybes
