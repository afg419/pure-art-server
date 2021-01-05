{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.WS where

import Import hiding (undefined, natVal, print)
import Foundation
import PointGen
import Data.Aeson
import Yesod.WebSockets
import Network.WebSockets (Connection)
import Model
import Handler.WSTypes
import UnliftIO.Concurrent

getHelloR :: Handler ()
getHelloR = do
    webSockets handleWs

handleWs :: WebSocketsT Handler ()
handleWs = forever $ do
    d <- receiveData
    _ <- liftIO <<$ print d
    case eitherDecode d of
      Left e -> sendJSONData $ rawExceptionResponse 400 ("Could not parse: " <> tshow e)
      Right w -> handleWsMessage w

handleWsMessage :: WsMessage -> WebSocketsT Handler ()
handleWsMessage w = do
  case msgAction $ debug "handling..." w of
    Approximate -> case extractApproximationIn w of
      Error e -> sendJSONData $ exceptionResponse w 400 (pack e)
      Success wsin -> do
        _ <- forkIO (doTheThing True w <<< approximations <<$ wsin)
        pure ()
    Cancel -> sendJSONData $ exceptionResponse w 400 "Cancellation not yet supported :("
    _ -> sendJSONData $ exceptionResponse w 400 "Unsupported message action"

counters :: [Natural]
counters = 0 : fmap (+1) counters

approximations :: WsApproximationIn -> [WsApproximationOut]
approximations wsin = mapMaybe (withContext wsin vertexApproximationFor) counters

doTheThing :: (MonadIO m, MonadReader Connection m) => Bool -> WsMessage -> [WsApproximationOut] -> m ()
doTheThing _ _ [] = pure ()
doTheThing _ _ [_] = pure ()
doTheThing firstTime wsin (a:b:cs) = do
  when firstTime $ sendJSONData respondA
  when (distanceA > distanceB) $ sendJSONData respondB

  if distanceA == 0 || distanceB == 0
    then finish
    else if distanceA > distanceB
      then do
        doTheThing False wsin (b:cs)
      else doTheThing False wsin (a:cs)
  where
    finish = pure ()
    respondA = swapContent wsin a
    respondB = swapContent wsin b
    distanceA = approxOutDistanceFromTarget a
    distanceB = approxOutDistanceFromTarget b

vertexApproximationFor :: (SContext a m n, ValidForContext a m n WsApproximationIn) -> Natural -> Maybe WsApproximationOut
vertexApproximationFor (sctx, wsin) counter = do
  let p = mkPath [0, counter]
  locale <- deriveLocale sctx xpub p
  pure $ WsApproximationOut p $ debug "running..." $ l1Dist wsin locale
  where
    xpub = approxInXpub $ unwrapValidContext wsin

sendJSONData :: (MonadIO m, ToJSON a, MonadReader Connection m) => a -> m ()
sendJSONData = sendTextData <<< debug "response!" <<< encode