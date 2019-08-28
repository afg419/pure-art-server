{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Paint where

import Import hiding (undefined)
import Effects.CanvasGeneration
import Effects.Common
import Effects.Interpreters
import PointGen
import Paint
import Data.Singletons
import Data.Aeson
import Model
import GHC.TypeLits.Witnesses

data PaintScaffoldReq (m :: Nat) (n :: Nat) = PaintScaffoldReq
  { xpub :: XPub
  , canvasId :: Canvas2Id
  , image :: Graph (Coordinate2 m n)
  , asset :: Asset
  } deriving (Eq, Show, Generic)

instance KnownNats m n => FromJSON (PaintScaffoldReq m n)

data PaintScaffoldRes (m :: Nat) (n :: Nat) where
  PaintScaffoldRes :: (KnownNats m n) => [TxScaffold (Locale a m n)] -> PaintScaffoldRes m n

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

scaffoldPaintLogic :: (CanvasGeneration r, KnownNats m n)
  => PaintScaffoldReq m n
  -> Effectful (Interpreter r) (PaintScaffoldRes m n)
scaffoldPaintLogic (PaintScaffoldReq xpub canvasId image asset) = do
  pure $ PaintScaffoldRes []

-- withNats :: (Natural, Natural) -> (forall m n. (Sing m, Sing n) -> r) -> r
-- withNats (i, j) f = withSomeNat i (\si -> withSomeNat j (\sj -> f (si, sj)))



scaffoldPaintLogic' :: forall m n r. (KnownNats m n, CanvasGeneration r) => PaintScaffoldReq m n -> r (Either String (PaintScaffoldRes m n))
scaffoldPaintLogic' (PaintScaffoldReq xpub canvasId image asset) = pure $ Right $ PaintScaffoldRes []
  mCanvas2 :: Maybe (SCanvas2 m n) <- getCanvas2 (SCanvas2Id canvasId)
  withSomeSing asset $ \sAsset -> pure $ do
       SCanvas2 (Canvas2{..}) <- mToE ("Canvas not found for id: " <> (show canvasId)) mCanvas2
       let hotL = hotLocale sAsset xpub


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
