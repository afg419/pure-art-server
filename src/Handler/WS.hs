{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.WS where

import Import hiding (undefined, natVal, print)
import Foundation
import PointGen
import Data.Aeson
import Yesod.WebSockets
import Network.WebSockets (Connection)
import Model

getHelloR :: Handler ()
getHelloR = do
    webSockets approximateVertices

approximateVertices :: WebSocketsT Handler ()
approximateVertices = forever $ do
    d <- receiveData
    _ <- liftIO <<$ print d
    case eitherDecode d of
      Left e -> sendJSONData $ WsExceptionOut ("Could not parse: " <> tshow e) 400
      Right (WsMessage {..} :: WsMessage WsApproximationIn) -> do
        doTheThing True <<< approximations <<$ msgContent

counters :: [Natural]
counters = 0 : fmap (+1) counters

approximations :: WsApproximationIn -> [WsApproximationOut]
approximations wsin = mapMaybe (withContext wsin vertexApproximationFor) counters

doTheThing :: (MonadIO m, MonadReader Connection m) => Bool -> [WsApproximationOut] -> m ()
doTheThing _ [] = pure ()
doTheThing _ [_] = pure ()
doTheThing firstTime (a:b:cs) = do
  when firstTime $ sendJSONData a
  when (da > db) $ sendJSONData b

  if da == 0 || db == 0
    then finish
    else if da > db
      then do
        doTheThing False (b:cs)
      else doTheThing False (a:cs)
  where
    finish = pure ()
    da = debug "a" $ approxOutDistanceFromTarget a
    db = debug "b" $ approxOutDistanceFromTarget b

vertexApproximationFor :: (SContext a m n, ValidForContext a m n WsApproximationIn) -> Natural -> Maybe WsApproximationOut
vertexApproximationFor (sctx, wsin) counter = do
  let path = mkPath [0, counter]
  locale <- deriveLocale sctx xpub path
  pure $ WsApproximationOut path $ l1Dist wsin locale
  where
    xpub = approxInXpub $ unwrapValidContext wsin

sendJSONData :: (MonadIO m, ToJSON a, MonadReader Connection m) => a -> m ()
sendJSONData = sendTextData <<< encode

data WsMessage t = WsMessage
  { msgTopicId :: Text
  , msgContent :: t
  , msgVersion :: Natural
  }

instance ToJSON t => ToJSON (WsMessage t) where
  toJSON WsMessage {..} = object
    [ "topicId" .= msgTopicId
    , "content" .= msgContent
    , "version" .= msgVersion
    ]

instance FromJSON t => FromJSON (WsMessage t) where
  parseJSON = withObject "ws message" $ \o -> do
    msgTopicId <- o .: "topicId"
    msgContent <- o .: "content"
    msgVersion <- o .: "version"
    pure $ WsMessage {..}

-- inbound
data WsApproximationIn = WsApproximationIn
  { approxInVertex :: Coordinate
  , approxInDimensions :: (Natural, Natural)
  , approxInXpub :: XPub
  , approxInAsset :: Asset
  } deriving (Eq, Show)

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

-- outbound

data WsApproximationOut = WsApproximationOut
  { approxOutPath :: DerivationPath 
  , approxOutDistanceFromTarget :: Natural
  } deriving (Eq, Show)

instance ToJSON WsApproximationOut where
  toJSON WsApproximationOut {..} = object 
    [ "path" .= approxOutPath
    , "distanceFromTarget" .= approxOutDistanceFromTarget
    ]
data WsExceptionOut = WsExceptionOut
  { exceptionMessage :: Text
  , exceptionCode :: Natural
  }

instance ToJSON WsExceptionOut where
  toJSON WsExceptionOut {..} = object 
    [ "message" .= exceptionMessage
    , "code" .= exceptionCode
    ]
