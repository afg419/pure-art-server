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

data PaintScaffoldReq = InitCanvasGenerationReq
  { asset :: Asset
  , xpub :: XPub
  , planeStock :: PlaneStock
  } deriving (Eq, Show, Generic)

instance FromJSON InitCanvasGenerationReq

data InitCanvasGenerationRes = InitCanvasGenerationRes
  { canvasId :: Text, originAddress :: Text } deriving (Eq, Show, Generic)

instance ToJSON InitCanvasGenerationRes

postInitCanvasGenerationR :: Handler Value
postInitCanvasGenerationR = do -- pure $ InitCanvasGenerationRes "" ""
  canvasGenReq <- requireCheckJsonBody
  eRes <- runEffects (run @PsqlDB) $ initCanvasGenerationLogic canvasGenReq
  case eRes of
    Left _ -> throwString "fucked"
    Right res -> pure $ toJSON res

initCanvasGenerationLogic :: CanvasGeneration r
  => InitCanvasGenerationReq
  -> Effectful (Interpreter r) (Either String InitCanvasGenerationRes)
initCanvasGenerationLogic InitCanvasGenerationReq{..} = do
  i <- interpret <$> ask
  i $ withSomeSing asset $ \sAsset ->
        withPlaneStock planeStock $ \plane -> insertCanvas2 sAsset xpub plane
          $>> ( tshow *** tshow >>> uncurry InitCanvasGenerationRes
                $>> fmap >>> fmap
              )
