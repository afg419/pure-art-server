{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Canvas where

import Import
import Effects.CanvasGeneration
import Effects.Common
import Effects.Interpreters
import PointGen

data InitCanvasGenerationReq = InitCanvasGenerationReq
  { xpub :: XPub
  , xDim :: Natural
  , yDim :: Natural
  , asset :: Asset
  } deriving (Eq, Show, Generic)

instance FromJSON InitCanvasGenerationReq

data InitCanvasGenerationRes = InitCanvasGenerationRes
  { canvasId :: Text } deriving (Eq, Show, Generic)

instance ToJSON InitCanvasGenerationRes

postInitCanvasGenerationR :: Handler Value
postInitCanvasGenerationR = do -- pure $ InitCanvasGenerationRes "" ""
  canvasGenReq <- requireCheckJsonBody
  fmap toJSON (runEffects (run @PsqlDB) <<$ initCanvasGenerationLogic canvasGenReq)

-- TODO :: check that hotlocale is defined.
initCanvasGenerationLogic :: CanvasGeneration r
  => InitCanvasGenerationReq
  -> Effectful (Interpreter r) InitCanvasGenerationRes
initCanvasGenerationLogic InitCanvasGenerationReq{..} = do
  i <- interpret <$> ask
  i $ withCanvasTy (asset, xDim, yDim)
    $ flip insertCanvas2 xpub >>> fmap tshow >>> fmap InitCanvasGenerationRes
