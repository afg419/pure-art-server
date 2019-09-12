{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Paint where

import Import hiding (undefined, natVal)
import Effects.CanvasGeneration
import Effects.Interpreters
import Effects.Common
import PointGen
import Paint
import Model
import Control.Monad.Except

data PaintScaffoldReq = PaintScaffoldReq
  { canvasId :: Canvas2Id
  , image :: Graph Coordinate2
  } deriving Generic
instance FromJSON PaintScaffoldReq

data SPaintScaffoldReq a m n = SPaintScaffoldReq
  { cty :: CTY a m n
  , scid :: SCanvas2Id a m n
  , sc2 :: SCanvas2 a m n
  , sImage :: Graph (SCoordinate2 m n)
  }

data PaintScaffoldRes v where
  PaintScaffoldRes :: [TxScaffold (BranchCounter v)] -> PaintScaffoldRes v
deriving instance Show v => (Show (PaintScaffoldRes v))
deriving instance (Functor PaintScaffoldRes)
instance ToJSON v => ToJSON (PaintScaffoldRes v) where
  toJSON (PaintScaffoldRes scaffoldList) = toJSON scaffoldList

postScaffoldPaintR :: Handler Value
postScaffoldPaintR = do
  canvasGenReq <- requireCheckJsonBody
  eRes <- runEffects (run @PsqlDB) <<< liftEffectful <<$ scaffoldBestFitPaintLogic canvasGenReq
  either (sendResponseStatus status500) pure eRes

mkSGraph :: CTY a m n
  -> Graph Coordinate2
  -> Maybe (Graph (SCoordinate2 m n))
mkSGraph cty graph = traverse testVertextOOB graph
  where
    testVertextOOB v = mkCoordinate (fst v) (snd v) (dim cty)

scaffoldBestFitPaintLogic :: (CanvasGeneration r) => PaintScaffoldReq -> r (Either Text Value)
scaffoldBestFitPaintLogic (PaintScaffoldReq cid image) = runExceptT $ do
    c2 <- ExceptT <<< fmap (mToE "Canvas not found") <<$ getCanvas2 cid

    withCanvasTy (canvasToCanvasTY c2) $ \cty -> do
      sc2 <- ExceptT <<< pure <<< mToE "incoherent canvas dimensions" <<$ mkSCanvas2 cty c2
      sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<$ mkSGraph cty image
      res <- ExceptT <<< fmap Right <<$ sScaffoldBestFitPaintLogic (SPaintScaffoldReq cty (SCanvas2Id cid) sc2 sImage)
      pure <<< toJSON <<$ res

sScaffoldBestFitPaintLogic :: (CanvasGeneration r) => SPaintScaffoldReq a m n -> r (PaintScaffoldRes (SLocale a m n))
sScaffoldBestFitPaintLogic (SPaintScaffoldReq cty scid (SCanvas2 c2) sImage) = do
  locales <- getPlane2Locales cty scid
  let localesImage = fmap (getClosestLocale locales) sImage
  let hotL = hotLocale cty xpub
  pure <<< PaintScaffoldRes <<$ graphToTxScaffold hotL localesImage
  where
    getClosestLocale ls p = minOn (\l -> l1Dist (lCoordinate l) p) ls
    xpub = canvas2Xpub c2
