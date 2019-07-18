{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Canvas where

import Import
import Effects.CanvasGeneration
import Effects.Common
import Effects.Interpreters
import Model
import PointGen
import Data.Singletons

data InitCanvasGenerationReq = InitCanvasGenerationReq
  { asset :: Asset
  , xpubId :: SXPubId
  , planeStock :: PlaneStock
  } deriving (Eq, Show, Generic)

instance FromJSON InitCanvasGenerationReq

data InitCanvasGenerationRes = InitCanvasGenerationRes
  { canvasId :: Text, originAddress :: Text }

postInitCanvasGenerationR :: Handler InitCanvasGenerationRes
postInitCanvasGenerationR = do -- pure $ InitCanvasGenerationRes "" ""
  canvasGenReq <- requireCheckJsonBody
  eRes <- runEffects (run @PsqlDB) $ initCanvasGenerationLogic canvasGenReq
  case eRes of
    Left _ -> throwString "fucked"
    Right (someId, someAddr) -> pure $ InitCanvasGenerationRes (tshow someId) (tshow someAddr)

initCanvasGenerationLogic :: CanvasGeneration r => InitCanvasGenerationReq -> Effectful (Interpreter r) (Either String (SomeCanvasId, SomeAddress))
initCanvasGenerationLogic InitCanvasGenerationReq{..} = do
  i <- interpret <$> ask
  pure <<< i $ withSomeSing asset $ \sAsset ->
        withPlaneStock planeStock $ \plane ->
          fmap (CanvasIdExists *** AddressExists) <$> insertCanvas2 sAsset xpubId plane
