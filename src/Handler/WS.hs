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

        sendJSONData $ WsApproximationOut (mkPath [0,0]) 0
      _ -> sendJSONData $ WsExceptionOut "Unsupported message" 400
    -- sendTextData ("Welcome to the chat server, please enter your name." :: Text)
    -- name <- receiveData
    -- sendTextData $ "Welcome, " <> name
    -- App writeChan <- getYesod
    -- readChan <- atomically $ do
    --     writeTChan writeChan $ name <> " has joined the chat"
    --     dupTChan writeChan
    -- race_
    --     (forever $ atomically (readTChan readChan) >>= sendTextData)
    --     (sourceWS $$ mapM_C (\msg ->
    --         atomically $ writeTChan writeChan $ name <> ": " <> msg))


approximateVertex :: WsApproximationIn -> Natural -> IO ()
approximateVertex wsin counter = do
  withContext wsin (vertexApproximationFor counter)



vertexApproximationFor :: Natural -> (SContext a m n, ValidForContext a m n WsApproximationIn) -> Maybe WsApproximationOut
vertexApproximationFor counter (sctx, wsin) = do
  let path = mkPath [0, counter]
  locale <- deriveLocale sctx xpub path
  let approxOutDistanceFromTarget = l1Dist wsin locale
  pure $ WsApproximationOut path approxOutDistanceFromTarget
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
  }

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
  }

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
