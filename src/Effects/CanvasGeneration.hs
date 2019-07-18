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
import Import (liftIO, getCurrentTime, (<<<), pack)

originAddressPath :: DerivationPath
originAddressPath = mkPath [0,0]

class Effect s => CanvasGeneration s where
  -- returns id of record, and 0/0 address
  insertCanvas2 :: KnownNats m n => SAsset a -> SXPubId -> Plane2 m n -> s (Either String (SCanvas2Id m n a, Address a))
  -- lookup hotwallet, if sufficient funds begin generating addresses
  startGeneration :: KnownNats m n => SCanvas2Id m n a -> s (Either String ())
  -- (missing coordinates, found coordinates)
  getPlane2Locales :: KnownNats m n => SCanvas2Id m n a -> s (Either String ([Coordinate m n] ,[Locale m n a]))

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
        now
        now
  startGeneration (SCanvas2Id canvasId) = undefined
  getPlane2Locales = undefined
