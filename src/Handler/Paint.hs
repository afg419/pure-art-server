{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Paint where

import Import hiding (undefined, natVal)
import Foundation
import Effects.Interpreters
import Effects.Paintings
import Effects.Common
import PointGen
import Paint
import Model
import Data.Aeson

postSubmitPaintR :: XPub -> Handler Value
postSubmitPaintR xpub = do
  app <- getYesod
  eSubmitPaintReq <- parseCheckJsonBody

  case eSubmitPaintReq of
    Error err -> pure $ String $ pack err
    Success submitPaintReq -> do
      eRes <-  flip runReaderT app <<< interpret (run @PsqlDB) <<$ submitPaintLogic xpub submitPaintReq
      either (sendResponseStatus status500) (pure <<< toJSON) eRes

submitPaintLogic :: Paintings s => XPub -> SubmitPaintReq -> s (Either Text SubmitPaintRes)
submitPaintLogic xpub req = do
    withContext req $ \(scty,_) -> do
      case mToE "vertices oob" <<< fmap SPainting <<$ mkSGraph (dim scty) (submitImage req) of
        Left e -> pure <<< Left <<$ e
        Right sPainting2 -> do
          sPaintingId <- insertPainting scty xpub sPainting2
          pure <<< Right <<< SubmitPaintRes <<< unwrapValidContext <<$ sPaintingId

data SubmitPaintReq = SubmitPaintReq
  { submitImage :: Graph Coordinate
  , submitAsset :: Asset
  , submitXSize :: Natural
  , submitYSize :: Natural
  } deriving (Generic, Show)

instance PlaneLike SubmitPaintReq where
  getXDim = fromIntegral <<< submitXSize
  getYDim = fromIntegral <<< submitYSize

instance HasContext SubmitPaintReq where
  getAsset = submitAsset

instance CanBeValid SubmitPaintReq where
  mkSafe c@(SContext a p) req =
    if assetValid && planeValid
    then Just $ ValidForContext a (ValidForPlane req)
    else Nothing
    where
      assetValid = getAsset c == submitAsset req
      planeValid = getXDim p == fromIntegral (submitXSize req) && getYDim p == fromIntegral (submitYSize req)

instance FromJSON SubmitPaintReq where
  parseJSON = withObject "submit painting request" $ \o -> do
    submitImage <- o .: "image"
    submitXSize <- o .: "xSize"
    submitYSize <- o .: "ySize"
    submitAsset <- o .: "asset"
    pure SubmitPaintReq{..}

newtype SubmitPaintRes = SubmitPaintRes { submitPaintingId :: PaintingRecordId }

instance ToJSON SubmitPaintRes where
  toJSON res = object [ "paintingId" .= submitPaintingId res ]



-- postSavePaintR :: XPub -> Handler Value
-- postSavePaintR xpub = do
--   savePaintReq <- requireCheckJsonBody
--   eRes <- runEffects (run @PsqlDB) <<< liftEffectful <<$ savePaintingLogic xpub savePaintReq
--   either (sendResponseStatus status500) pure eRes

-- getRetrievePaintR :: XPub -> PaintingRecordId -> Handler Value
-- getRetrievePaintR xpub prid = do
--   eRes <- runEffects (run @PsqlDB) <<< liftEffectful <<$ retreivePaintingLogic xpub prid
--   either (sendResponseStatus status500) pure eRes
--
-- retreivePaintingLogic :: (Paintings s, CanvasGeneration s) => XPub -> PaintingRecordId -> s (Either Text Value)
-- retreivePaintingLogic xpub prid = runExceptT $ do
--     _ <- ExceptT <<< fmap (mToE "Xpub not found") <<$ getPublicKeyGeneratorId xpub
--     fmap toJSON <<< ExceptT <<< fmap (mToE "Painting not found") <<$ retrievePainting prid
--
-- data SavePaintReq = SavePaintReq
--   { image :: Graph Coordinate
--   , xSize :: Natural
--   , ySize :: Natural
--   } deriving Generic
-- instance FromJSON SavePaintReq
--
-- savePaintingLogic :: (Paintings s, CanvasGeneration s) => XPub -> SavePaintReq -> s (Either Text Value)
-- savePaintingLogic xpub (SavePaintReq image) = runExceptT $ do
--     pkgenId <- ExceptT <<< fmap (mToE "Xpub not found") <<$ getPublicKeyGeneratorId xpub
--
--     withPlaneTy (cx, cy) $ \p2 -> do
--       sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<< fmap SPainting <<$ mkSGraph p2 image
--       res <- lift <<$ insertPainting pkgenId p2 sImage
--       pure <<< toJSON <<$ res

-- postScaffoldPaintR :: Handler Value
-- postScaffoldPaintR = do
--   canvasGenReq <- requireCheckJsonBody
--   eRes <- runEffects (run @PsqlDB) <<< liftEffectful <<$ scaffoldBestFitPaintLogic canvasGenReq
--   either (sendResponseStatus status500) pure eRes

-- data PaintScaffoldReq = PaintScaffoldReq
--   { paintingRecordId :: PaintingRecordId
--   , asset :: Asset
--   } deriving Generic
-- instance FromJSON PaintScaffoldReq
--
-- data SPaintScaffoldReq a m n = SPaintScaffoldReq
--   (SContext a m n)
--   (Entity PublicKeyGenerator)
--   (Graph (SCoordinate m n))
--
-- data PaintScaffoldRes v where
--   PaintScaffoldRes :: [TxScaffold (BranchCounter v)] -> PaintScaffoldRes v
-- deriving instance Show v => (Show (PaintScaffoldRes v))
-- deriving instance (Functor PaintScaffoldRes)
-- instance ToJSON v => ToJSON (PaintScaffoldRes v) where
--   toJSON (PaintScaffoldRes scaffoldList) = toJSON scaffoldList
--
-- data DpSLocale (a :: Asset) (m :: Nat) (n :: Nat) = DpSLocale
--   { dPath :: DerivationPath
--   , sLocale :: (SLocale a m n)
--   } deriving (Show, Eq)
--
-- instance ToJSON (DpSLocale a m n) where
--   toJSON (DpSLocale dp SLocale{..}) = object
--     [ "coordinate" .= array [cx lCoordinate, cy lCoordinate]
--     , "address" .= (String $ tshow lAddress)
--     , "path" .= (String $ tshow dp)
--     ]

mkSGraph :: Plane m n
  -> Graph Coordinate
  -> Maybe (Graph (SCoordinate m n))
mkSGraph p2 = traverse testVertextOOB
  where
    testVertextOOB v = mkSafeCoordinate p2 v

-- scaffoldBestFitPaintLogic :: (CanvasGeneration r, Paintings r) => PaintScaffoldReq -> r (Either Text Value)
-- scaffoldBestFitPaintLogic (PaintScaffoldReq paintingId ca) = runExceptT $ do
--
--     pkgen <- ExceptT <<< fmap (mToE "Xpub not found") <<$ getPublicKeyGenerator xpub
--
--     withContext cty $ \scty -> do
--       sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<$ mkSGraph (dim scty) image
--       res <- ExceptT <<< fmap Right <<$ sScaffoldBestFitPaintLogic (SPaintScaffoldReq scty pkgen sImage)
--       pure <<< toJSON <<$ res
--
--     where
--       cty = HasContext ca cx cy
--
-- sScaffoldBestFitPaintLogic :: (CanvasGeneration r) => SPaintScaffoldReq a m n -> r (PaintScaffoldRes (DpSLocale a m n))
-- sScaffoldBestFitPaintLogic (SPaintScaffoldReq scty pkgen sImage) = do
--   locales <- fmap (fmap toDpsLocale) <<< getPublicKeys <<$ pkgenId
--
--   let localesImage = fmap (getClosestLocale locales) sImage
--   let hotL = DpSLocale hotPath (hotLocale scty xpub)
--   pure <<< PaintScaffoldRes <<$ graphToTxScaffold hotL localesImage
--   where
--     pkgenId = entityKey pkgen
--     xpub = publicKeyGeneratorXpub <<$ entityVal pkgen
--     getClosestLocale ls p = minOn (\l -> l1Dist (lCoordinate <<$ sLocale l) p) ls
--     toDpsLocale PublicKeyRecord{..} =
--       DpSLocale
--       publicKeyRecordPath
--       (mkLocale scty publicKeyRecordPublicKey)
