{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.WS where

import Import hiding (undefined, natVal, print)
import Foundation
import PointGen
import Data.Aeson
import Yesod.WebSockets

getHelloR :: Handler ()
getHelloR = do
    webSockets chatApp

chatApp :: WebSocketsT Handler ()
chatApp = forever $ do
    d <- receiveData
    _ <- liftIO <<$ print d
    case decode d of
      Nothing -> sendTextData $ encode ("eat it!" :: Text)
      Just ApproximationMessage {..} -> sendTextData $ encode approximationSubsciptionId

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

data ApproximationMessage = ApproximationMessage
  { approximationSubsciptionId :: Text
  , approximationVertex :: Coordinate
  , approximationDimensions :: (Natural, Natural)
  , approximationXpub :: XPub
  }

instance FromJSON ApproximationMessage where
  parseJSON = withObject "approximation message" $ \o -> do
    approximationSubsciptionId <- o .: "subscribe"
    approximationVertex <- o .: "v"
    approximationXpub <- o .: "xpub"
    approximationDimensions <- o .: "dim"
    pure $ ApproximationMessage { .. }

-- instance W.WebSocketsData (Maybe ApproximationMessage) where
--   fromDataMessage (W.Text t _) = decode t
--   fromDataMessage (W.Binary b) = decode <<< decodeUtf8 <<$ b
