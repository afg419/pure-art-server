{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.RetrievePaint where

import Import hiding (undefined, natVal)
import Foundation
import Effects.Interpreters
import Effects.Paintings
import Effects.Common
import PointGen
import Paint
import Model
import Data.Aeson

getRetrievePaintR :: XPub -> PaintingRecordId -> Handler Value
getRetrievePaintR xpub prid = do
  app <- getYesod
  pure _

data RetrievePaintRes v = RetrievePaintRes
  { retrieveScaffold :: [TxScaffold (BranchCounter v)] }

retrievePaintLogic :: Paintings s => XPub -> PaintingRecordId -> s (Either Text (RetrievePaintRes LocaleRecord))
retrievePaintLogic xpub prid = do
  mPainting <- retrievePainting prid
  case mPainting of
    Nothing -> pure <<< Left <<$ "Painting Record Not Found"
    Just p -> pure _

-- submitPaintLogic :: Paintings


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
--   { image :: Graph Coordinate2
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
--       sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<< fmap SPainting2 <<$ mkSGraph p2 image
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
--   (SCTY a m n)
--   (Entity PublicKeyGenerator)
--   (Graph (SCoordinate2 m n))
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

mkSGraph :: Plane2 m n
  -> Graph Coordinate2
  -> Maybe (Graph (SCoordinate2 m n))
mkSGraph p2 graph = traverse testVertextOOB graph
  where
    testVertextOOB v = mkCoordinate (fst v) (snd v) p2

-- scaffoldBestFitPaintLogic :: (CanvasGeneration r, Paintings r) => PaintScaffoldReq -> r (Either Text Value)
-- scaffoldBestFitPaintLogic (PaintScaffoldReq paintingId ca) = runExceptT $ do
--
--     pkgen <- ExceptT <<< fmap (mToE "Xpub not found") <<$ getPublicKeyGenerator xpub
--
--     withCanvasTy cty $ \scty -> do
--       sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<$ mkSGraph (dim scty) image
--       res <- ExceptT <<< fmap Right <<$ sScaffoldBestFitPaintLogic (SPaintScaffoldReq scty pkgen sImage)
--       pure <<< toJSON <<$ res
--
--     where
--       cty = CTY ca cx cy
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
