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
  insertCanvas2 :: KnownNats m n => SAsset a -> XPub -> Plane2 m n -> s (Either String (SCanvas2Id m n a, Address a))
  getCanvas2    :: (SingI a, KnownNats m n) => SCanvas2Id m n a -> s (Maybe (SCanvas2 m n a, SAsset a))
  updateCanvas2NextPathIndex :: SCanvas2Id m n a -> Integer -> s ()

  -- this uses a repsertMany so no duplicate coordinates stored
  insertPlane2Locales :: KnownNats m n => SCanvas2Id m n a -> [Locale m n a] -> s ()
  getPlane2Locales :: (SingI a, KnownNats m n) => SCanvas2Id m n a -> s [Locale m n a]

instance CanvasGeneration PsqlDB where
  insertCanvas2 sAsset xpub p2 = do
    let originAddress = lAddress <<< fromJust $ deriveLocale sAsset xpub p2 originAddressPath
    now <- liftIO getCurrentTime
    canvas2Id <- PsqlDB <<< insert $ canvas (originAddress, now)
    pure <<< Right $ (SCanvas2Id canvas2Id, originAddress)
      where
        (xSize, ySize) = plane2Dim p2
        canvas = \(originAddress, now) -> Canvas2
          xpub
          (fromSing sAsset)
          (fromIntegral xSize)
          (fromIntegral ySize)
          (pack <<< show $ originAddress)
          0
          now
          now
  getCanvas2 (SCanvas2Id cId) = do
    mCanvas2 <- PsqlDB <<< get $ cId
    pure <<< fmap (( , sing) <<< SCanvas2) $ mCanvas2
  updateCanvas2NextPathIndex (SCanvas2Id cId) nextIndex = [Canvas2NextPathIndex =. (fromIntegral nextIndex)]
    $>> update cId
    >>> PsqlDB
  insertPlane2Locales scId locales = do
    now <- liftIO getCurrentTime
    PsqlDB <<< repsertMany $ fmap ( toLocaleRecordKey scId &&& toLocaleRecord scId now) locales
  getPlane2Locales scid@(SCanvas2Id cid) =
    selectList [ LocaleRecordCanvas2Id ==. cid] []
    $>> fmap (fmap $ entityVal >>> fromLocaleRecord sing scid)
    >>> PsqlDB
