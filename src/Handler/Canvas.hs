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
import Data.Singletons

data InitCanvasGenerationReq = InitCanvasGenerationReq
  { xpub :: XPub
  , planeStock :: PlaneStock
  , asset :: Asset
  } deriving (Eq, Show, Generic)

instance FromJSON InitCanvasGenerationReq

data InitCanvasGenerationRes = InitCanvasGenerationRes
  { canvasId :: Text } deriving (Eq, Show, Generic)

instance ToJSON InitCanvasGenerationRes

postInitCanvasGenerationR :: Handler Value
postInitCanvasGenerationR = do -- pure $ InitCanvasGenerationRes "" ""
  canvasGenReq <- requireCheckJsonBody
  eRes <- runEffects (run @PsqlDB) $ initCanvasGenerationLogic canvasGenReq
  case eRes of
    Left _ -> throwString "fucked"
    Right res -> pure $ toJSON res

-- TODO :: check that hotlocale is defined.
initCanvasGenerationLogic :: CanvasGeneration r
  => InitCanvasGenerationReq
  -> Effectful (Interpreter r) (Either String InitCanvasGenerationRes)
initCanvasGenerationLogic InitCanvasGenerationReq{..} = do
  i <- interpret <$> ask
  i $ withPlaneStock planeStock $ \plane ->
    withSomeSing asset $ \sAsset ->
      insertCanvas2 xpub sAsset plane
            $>> ( tshow >>> InitCanvasGenerationRes $>> fmap >>> fmap )
