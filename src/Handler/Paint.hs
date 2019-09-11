{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Paint where

import Import hiding (undefined, Proxy, natVal)
import Effects.CanvasGeneration
import Effects.Common
import Effects.Interpreters
import PointGen
import Paint
import Data.Singletons
import Data.Aeson
import Model
import Data.Singletons.TypeLits
import Control.Monad.Except

-- import GHC.TypeNats

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

-- scaffoldPaintLogic :: (CanvasGeneration r, KnownNats m n)
--   => PaintScaffoldReq m n
--   -> Effectful (Interpreter r) (PaintScaffoldRes m n)
-- scaffoldPaintLogic (PaintScaffoldReq xpub canvasId image asset) = do
--   pure $ PaintScaffoldRes []
--
-- -- withNats :: (Natural, Natural) -> (forall m n. (Sing m, Sing n) -> r) -> r
-- -- withNats (i, j) f = withSomeNat i (\si -> withSomeNat j (\sj -> f (si, sj)))
--
--
--
-- scaffoldEstimationPaintLogic :: PaintScaffoldReq -> Either Text (PaintScaffoldRes (SCoordinate2 m n))
-- scaffoldEstimationPaintLogic (PaintScaffoldReq cid image) = do
--   mCanvas2 <- getCanvas2 cid
--   case mCanvas2 of
--     Nothing -> pure $ Left "Canvas not found"
--     Just c2 ->
--       sequenceA $ withPromotedEither
--           (c2, image)
--           paintScaffoldReqDecomp
--           mkScaffoldReq
--           sScaffoldBestFitPaintLogic

mkSCanvas2 :: KnownNats m n => CTY a m n -> Canvas2 -> Maybe (SCanvas2 a m n)
mkSCanvas2 (CTY (sa, sx, sy)) c2 = if (fromSing sa, natVal sx, natVal sy) == canvasToCanvasTY c2
  then Just (SCanvas2 c2)
  else Nothing

mkScaffoldReq :: forall m n a. KnownNats m n
  => CTY a m n
  -> Graph Coordinate2
  -> Maybe (Graph (SCoordinate2 m n))
mkScaffoldReq ty graph = traverse testVertextOOB graph
  where
    testVertextOOB v = mkCoordinate (fst v) (snd v) ty


scaffoldBestFitPaintLogic :: (CanvasGeneration r) => PaintScaffoldReq -> r (Either Text (PaintScaffoldRes Locale))
scaffoldBestFitPaintLogic (PaintScaffoldReq cid image) = runExceptT $ do
    c2 <- ExceptT <<< fmap (mToE "Canvas not found") <<$ getCanvas2 cid

    withCanvasTy (canvasToCanvasTY c2) $ \cty -> do
      sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<$ mkScaffoldReq cty image
      res <- ExceptT <<< fmap Right <<$ sScaffoldBestFitPaintLogic cty (SPaintScaffoldReq c2 cid sImage)
      undefined

  where
    x :: CTY a m n -> r (Either Text (PaintScaffoldRes Locale))
    x = error "shit"
      -- sequenceA $ withPromotedEither
      --     (c2, image)
      --     paintScaffoldReqDecomp
      --     mkScaffoldReq
      --     sScaffoldBestFitPaintLogic

-- sScaffoldEstimationPaintLogic :: (SingI a, CanvasGeneration r, KnownNats m n) => SPaintScaffoldReq a m n -> r PaintScaffoldRes
-- sScaffoldEstimationPaintLogic (SPaintScaffoldReq _ _ sImage) = do
--   locales <- getPlane2Locales scid
--   let localesImage = fmap (getClosestLocale locales) sImage
--   let hotL = hotLocale sAsset xpub P2
--   pure <<< PaintScaffoldRes <<$ graphToTxScaffold hotL localesImage
--   where
--     getClosestLocale ls point = minOn (\l -> l1Dist (lCoordinate l) point) ls
--     sAsset = sing
--     xpub = canvas2Xpub c2

data SPaintScaffoldReq m n = SPaintScaffoldReq
  { c2 :: Canvas2
  , cid :: Canvas2Id
  , sImage :: Graph (SCoordinate2 m n)
  }

sScaffoldBestFitPaintLogic :: (CanvasGeneration r, KnownNats m n) => CTY a m n -> SPaintScaffoldReq m n -> r (PaintScaffoldRes (SLocale a m n))
sScaffoldBestFitPaintLogic (CTY (sAsset, _, _) )(SPaintScaffoldReq c2 cid sImage) = do
  locales <- getPlane2Locales sAsset cid
  let localesImage = fmap (getClosestLocale locales) sImage
  let hotL = hotLocale sAsset xpub P2
  pure <<< PaintScaffoldRes <<$ graphToTxScaffold hotL localesImage
  where
    getClosestLocale ls point = minOn (\l -> l1Dist (lCoordinate l) point) ls
    xpub = canvas2Xpub c2

canvasToCanvasTY :: Canvas2 -> (Asset, Natural, Natural)
canvasToCanvasTY Canvas2{..} =
  ( canvas2Asset
  , fromIntegral canvas2XSize
  , fromIntegral canvas2YSize
  )

-- scaffoldBestFitPaintLogic :: forall m n r. (KnownNats m n, CanvasGeneration r) => Graph (Coordinate2 m n) -> SAsset a -> SCanvas2 a m n -> r (Maybe (PaintScaffoldRes))
-- scaffoldPaintLogic (PaintScaffoldReq xpub scanvasId image sasset) = undefined
--
--
--        let hotL = hotLocale sAsset xpub plane2
--        locales <- getPlane2Locales sAsset (SCanvas2Id canvasId)
--        graphToTxScaffold hotL
--        PaintScaffoldRes []

-- scaffoldPaintLogic' :: forall m n r. (KnownNats m n, CanvasGeneration r) => PaintScaffoldReq m n -> r (Maybe (PaintScaffoldRes m n))
-- scaffoldPaintLogic' (PaintScaffoldReq xpub canvasId image asset) = withCanvasId canvasId $ \plane2 -> do
--   withSomeSing asset $ \sAsset -> do
--        let hotL = hotLocale sAsset xpub plane2
--        locales <- getPlane2Locales sAsset (SCanvas2Id canvasId)
--        graphToTxScaffold hotL
--        PaintScaffoldRes []
--

  -- do

  -- do $
  --




-- initCanvasGenerationLogic :: CanvasGeneration r
--   => InitCanvasGenerationReq
--   -> Effectful (Interpreter r) (Either String InitCanvasGenerationRes)
-- initCanvasGenerationLogic InitCanvasGenerationReq{..} = do
--   i <- interpret <$> ask
--   i $ withSomeSing asset $ \sAsset ->
--         withPlaneStock planeStock $ \plane -> insertCanvas2 sAsset xpub plane
--           $>> ( tshow *** tshow >>> uncurry InitCanvasGenerationRes
--                 $>> fmap >>> fmap
--               )
