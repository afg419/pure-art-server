{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Effects.CanvasGeneration where

import PointGen.Address
import PointGen.Plane
import PointGen.Coordinate
import PointGen.AddressTransform
import PointGen.Bip32
import Model
import Effects.Common
import Effects.Interpreters
import Database.Persist
import Data.Singletons
import Data.Maybe (fromJust)
import Import (liftIO, getCurrentTime, (<<<), pack, tshow)

originAddressPath :: DerivationPath
originAddressPath = mkPath [0,0]

class Effect s => CanvasGeneration s where
  -- returns id of record, and 0/0 address
  insertCanvas2 :: KnownNats m n => SAsset a -> SXPubId -> Plane2 m n -> s (Either String (SCanvas2Id m n a, Address a))
  getCanvas2    :: (SingI a, KnownNats m n) => SCanvas2Id m n a -> s (Maybe (SCanvas2 m n a, SAsset a))
  -- -- lookup hotwallet, if sufficient funds begin generating addresses
  -- (missing coordinates, found coordinates)
  getPlane2Locales :: KnownNats m n => SCanvas2Id m n a -> s (Either String [Locale m n a])
  ginsertPlane2Locale :: KnownNats m n => SCanvas2Id m n a -> Locale m n a -> s (SLocaleId m n a, GInsert) -- returns localeId for coordinate

instance CanvasGeneration PsqlDB where
  insertCanvas2 sAsset (SXPubId xpubId) p2 = do
    mXpubRecord <- PsqlDB <<< get $ xpubId
    case mXpubRecord of
      Nothing -> pure $ Left "No Xpub Found"
      Just xPubRecord -> do
        let originAddress = lAddress <<< fromJust $ deriveLocale sAsset (xPubRecordXpub xPubRecord) p2 originAddressPath
        now <- liftIO getCurrentTime
        canvas2Id <- PsqlDB <<< insert $ canvas (originAddress, now)
        pure <<< Right $ (SCanvas2Id canvas2Id, originAddress)
    where
      (xSize, ySize) = plane2Dim p2
      canvas = \(originAddress, now) -> Canvas2
        xpubId
        (fromSing sAsset)
        (fromIntegral xSize)
        (fromIntegral ySize)
        (pack <<< show $ originAddress)
        0
        now
        now
  ginsertPlane2Locale (SCanvas2Id canvasId) Locale{..} = do
    let targetX = fromIntegral $ x lCoordinate
    let targetY = fromIntegral $ y lCoordinate
    mAlreadyFound <- PsqlDB <<< selectFirst [LocaleRecordX ==. targetX, LocaleRecordY ==. targetY] $ []
    case mAlreadyFound of
      Just yep -> pure ((SLocaleId <<< entityKey $ yep), Get)
      Nothing -> do
        now <- liftIO getCurrentTime
        newKey <- PsqlDB <<< insert $ LocaleRecord canvasId targetX targetY lPath (tshow lAddress) now now
        pure (SLocaleId newKey, Insert)
  getPlane2Locales = undefined
  getCanvas2 (SCanvas2Id cId) = do
    mCanvas2 <- PsqlDB <<< get $ cId
    pure <<< fmap (( , sing) <<< SCanvas2) $ mCanvas2
