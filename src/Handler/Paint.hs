{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Paint where

import Import hiding (undefined, natVal)
import Effects.CanvasGeneration
import PointGen
import Paint
import Data.Aeson
import Model
import Control.Monad.Except

data PaintScaffoldReq = PaintScaffoldReq
  { canvasId :: Canvas2Id
  , image :: Graph Coordinate2
  }

data PaintScaffoldRes v where
  PaintScaffoldRes :: [TxScaffold (BranchCounter v)] -> PaintScaffoldRes v
deriving instance Show v => (Show (PaintScaffoldRes v))

postScaffoldPaintR :: Handler Value
postScaffoldPaintR = do -- pure $ InitCanvasGenerationRes "" ""
  -- canvasGenReq <- requireCheckJsonBody
  -- eRes <- runEffects (run @PsqlDB) $ scaffoldPaintLogic canvasGenReq
  pure $ Null
  -- canvasGenReq <- requireCheckJsonBody
  -- eRes <- runEffects (run @PsqlDB) $ initCanvasGenerationLogic canvasGenReq
  -- case eRes of
  --   Left _ -> throwString "fucked"
  --   Right res -> pure $ toJSON res

mkScaffoldReq :: CTY a m n
  -> Graph Coordinate2
  -> Maybe (Graph (SCoordinate2 m n))
mkScaffoldReq cty graph = traverse testVertextOOB graph
  where
    testVertextOOB v = mkCoordinate (fst v) (snd v) (dim cty)


scaffoldBestFitPaintLogic :: (CanvasGeneration r) => PaintScaffoldReq -> r (Either Text (PaintScaffoldRes Locale))
scaffoldBestFitPaintLogic (PaintScaffoldReq cid image) = runExceptT $ do
    c2 <- ExceptT <<< fmap (mToE "Canvas not found") <<$ getCanvas2 cid

    withCanvasTy (canvasToCanvasTY c2) $ \cty -> do
      sc2 <- ExceptT <<< pure <<< mToE "incoherent canvas dimensions" <<$ mkSCanvas2 cty c2
      sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<$ mkScaffoldReq cty image
      res <- ExceptT <<< fmap Right <<$ sScaffoldBestFitPaintLogic (SPaintScaffoldReq sc2 (SCanvas2Id cid) cty sImage)
      undefined

data SPaintScaffoldReq a m n = SPaintScaffoldReq
  { sc2 :: SCanvas2 a m n
  , scid :: SCanvas2Id a m n
  , cty :: CTY a m n
  , sImage :: Graph (SCoordinate2 m n)
  }

sScaffoldBestFitPaintLogic :: (CanvasGeneration r) => SPaintScaffoldReq a m n -> r (PaintScaffoldRes (SLocale a m n))
sScaffoldBestFitPaintLogic (SPaintScaffoldReq (SCanvas2 c2) scid cty sImage) = do
  locales <- getPlane2Locales cty scid
  let localesImage = fmap (getClosestLocale locales) sImage
  let hotL = hotLocale cty xpub
  pure <<< PaintScaffoldRes <<$ graphToTxScaffold hotL localesImage
  where
    getClosestLocale ls p = minOn (\l -> l1Dist (lCoordinate l) p) ls
    xpub = canvas2Xpub c2
