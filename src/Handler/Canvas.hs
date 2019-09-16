{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Canvas where

import Import
import Foundation
import Effects.CanvasGeneration
import Effects.Common
import Effects.Interpreters
import PointGen

data CreatePubkeyGeneratorReq = CreatePubkeyGeneratorReq
  { xpub :: XPub
  } deriving (Eq, Show, Generic)

instance FromJSON CreatePubkeyGeneratorReq

data CreatePubkeyGeneratorRes = CreatePubkeyGeneratorRes
  { publicKeyGeneratorId :: Text } deriving (Eq, Show, Generic)

instance ToJSON CreatePubkeyGeneratorRes

postCreatePubkeyGeneratorR :: Handler Value
postCreatePubkeyGeneratorR = do -- pure $ CreatePubkeyGeneratorRes "" ""
  canvasGenReq <- requireCheckJsonBody
  fmap toJSON (runEffects (run @PsqlDB) <<$ initCanvasGenerationLogic canvasGenReq)

-- TODO :: check that hotlocale is defined.
initCanvasGenerationLogic :: CanvasGeneration r
  => CreatePubkeyGeneratorReq
  -> Effectful (Interpreter r) CreatePubkeyGeneratorRes
initCanvasGenerationLogic CreatePubkeyGeneratorReq{..} = do
  i <- interpret <$> ask
  i $ do
    genId <- insertPublicKeyGenerator xpub
    pure <<< CreatePubkeyGeneratorRes <<$ tshow genId
