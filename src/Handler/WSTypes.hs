{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Handler.WSTypes where

import Import hiding (undefined, natVal, print)
import PointGen
import Data.Aeson
import Model
data MessageAction = Approximate | Cancel
  deriving (Show, Eq)

instance ToJSON MessageAction where
  toJSON Approximate = String "approximate"
  toJSON Cancel = String "cancel"
instance FromJSON MessageAction where
  parseJSON = withText "ws action" $ \case
    "approximate" -> pure Approximate
    "cancel" -> pure Cancel
    _ -> fail "unsupported action"

data WsMessage = WsMessage
  { msgTopicId :: Text
  , msgId :: Text
  , msgAction :: MessageAction
  , msgContent :: Value -- decode this later in a specific handler based on the action
  , msgVersion :: Natural
  } deriving (Eq, Show)

swapContent :: ToJSON a => WsMessage -> a -> WsMessage
swapContent ws a = ws { msgContent = toJSON a }

instance ToJSON WsMessage where
  toJSON WsMessage {..} = object
    [ "topicId" .= msgTopicId
    , "msgId"   .= msgId
    , "content" .= msgContent
    , "version" .= msgVersion
    , "action"  .= msgAction
    ]

instance FromJSON WsMessage where
  parseJSON = withObject "ws message" $ \o -> do
    msgTopicId <- o .: "topicId"
    msgId      <- o .: "id"
    msgContent <- o .: "content"
    msgVersion <- o .: "version"
    msgAction  <- o .: "action"
    pure $ WsMessage {..}

-- inbound
data WsApproximationIn = WsApproximationIn
  { approxInVertex :: Coordinate
  , approxInDimensions :: (Natural, Natural)
  , approxInXpub :: XPub
  , approxInAsset :: Asset
  } deriving (Eq, Show)

extractApproximationIn :: WsMessage -> Result WsApproximationIn
extractApproximationIn WsMessage{..} =
  if msgAction /= Approximate
    then Error $ "expected action approximate, got: " <> show msgAction
    else fromJSON msgContent

instance FromJSON WsApproximationIn where
  parseJSON = withObject "approx in" $ \o -> do
    approxInVertex <- o .: "vertex"
    approxInDimensions <- o .: "dimensions"
    approxInXpub <- o .: "xpub"
    approxInAsset <- o .: "asset"
    pure $ WsApproximationIn {..}

instance PlaneLike WsApproximationIn where
  getXDim WsApproximationIn{..} = fst approxInDimensions
  getYDim WsApproximationIn{..} = snd approxInDimensions
instance CoordinateLike WsApproximationIn where
  getX WsApproximationIn{..} = getX approxInVertex
  getY WsApproximationIn{..} = getY approxInVertex
instance HasContext WsApproximationIn where
  getAsset WsApproximationIn{..} = approxInAsset
instance CanBeValid WsApproximationIn where
  mkSafe c@(SContext a p) w@WsApproximationIn {..} =
    if assetValid && planeValid
    then Just $ ValidForContext a (ValidForPlane w)
    else Nothing
    where
      assetValid = getAsset c == getAsset w
      planeValid = getXDim p == getXDim w && getYDim p == getYDim w

data WsApproximationOut = WsApproximationOut
  { approxOutPath :: DerivationPath
  , approxOutDistanceFromTarget :: Natural
  } deriving (Eq, Show)

instance ToJSON WsApproximationOut where
  toJSON WsApproximationOut {..} = object
    [ "path" .= approxOutPath
    , "distanceFromTarget" .= approxOutDistanceFromTarget
    ]

exceptionResponse :: WsMessage -> Natural -> Text -> WsException
exceptionResponse WsMessage{..} excCode excMessage = let
    excId = Just msgId
    excTopicId = Just msgTopicId
  in WsException {..}

data WsException = WsException
  { excMessage :: Text
  , excCode :: Natural
  , excId :: Maybe Text
  , excTopicId :: Maybe Text
  }

instance ToJSON WsException where
  toJSON WsException {..} = object 
    [ "message" .= excMessage
    , "code" .= excCode
    , "id" .= excId
    , "topicId" .= excTopicId
    ]
