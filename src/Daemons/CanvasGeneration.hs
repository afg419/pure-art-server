{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Daemons.CanvasGeneration where

import PointGen
import Model
import Effects.Paintings
import Control.Monad.Trans.Except
import Import hiding (undefined, sum, filter, elem)
import Foundation
import UnliftIO.Concurrent (threadDelay)
import Effects.Interpreters
import Effects.Common

approximateVerticesLoop :: Handler ()
approximateVerticesLoop = forever $ do
  _ <- liftHandler <<< runEffects (run @PsqlDB) <<< liftEffectful $ do
    paintingEntities <- retrieveParitallyApproximatedPaintings

    for paintingEntities $ \paintingE -> do
      withCanvasTy paintingE $ \(scty, sPaintingE) -> do
        approximateVerticesWithLocales scty sPaintingE 1000

  threadDelay $ 1 * 10000000

data GenerateLocalesRes =
  LocalesComplete | LocalesImperfect Natural Natural Natural -- missing, imperfect, total

approximateVerticesWithLocales ::
  forall a m n r. Paintings r
  => SCTY a m n
  -> Safe (Entity PaintingRecord) a m n
  -> Natural
  -> r (Either Text GenerateLocalesRes)
approximateVerticesWithLocales scty sPrec totalTries = runExceptT $ do
  let painting = fromSafe <<$ sEntityVal sPrec
  let sPrid = sEntityKey sPrec
  vertices <- lift <<$ retrievePaintingVertices (dim scty) sPrid

  let nextTry = fromIntegral (paintingRecordNextPathIndex painting)
  let tries = [nextTry .. nextTry + totalTries - 1]
  let pathsToTry = pathsForIndices tries
  let xpub = paintingRecordXPub painting

  let locales = mapMaybe (deriveLocale scty xpub) pathsToTry

  _ <- lift <<< for locales <<$ replaceVertexLocaleIfBetterApproximation vertices

  lift <<$ updatePaintingIndex (fromSafe sPrid) (nextTry + totalTries)

  updatedVertices <- lift <<$ retrievePaintingVertices (dim scty) sPrid
  case classifyVertices updatedVertices of
    LocalesComplete -> do
      lift <<$ markPaintingFullyApproximated (fromSafe sPrid)
      pure LocalesComplete
    l -> pure l


replaceVertexLocaleIfBetterApproximation :: Paintings s => [Safe VertexRecord a m n] -> SLocale a m n -> s ()
replaceVertexLocaleIfBetterApproximation vertices l = do
  let vertex = minOn (l1Dist l) vertices
  let candidateDistance = l1Dist l vertex
  let previousClosestLocale = vertexRecordClosestLocale <<$ fromSafe vertex

  case previousClosestLocale of
    Nothing -> replaceVertexLocale vertex l
    Just prevLocale -> if candidateDistance < l1Dist prevLocale vertex
      then replaceVertexLocale vertex l
      else pure ()

classifyVertices :: [Safe VertexRecord a m n] -> GenerateLocalesRes
classifyVertices vertices = if (missingApproximations + imperfectApproximations > 0)
    then LocalesImperfect missingApproximations imperfectApproximations totalVertices
    else LocalesComplete
  where
    classifiedVertices = fmap classifyVertexRecord vertices
    missingApproximations = fromIntegral <<< length <<$ filter (== NonExistent) classifiedVertices
    imperfectApproximations = fromIntegral <<< length <<$ filter (== Approximate) classifiedVertices
    totalVertices = fromIntegral <<< length <<$ classifiedVertices

-- On average for a fair n-sided dice it takes n * sum_k=1^n 1/k
estimateAttemptsNeededForEntireCanvas :: (Ord a, Floating a) => Plane2 m n -> Range a
estimateAttemptsNeededForEntireCanvas p = mkRange (diceSides * lowBoundSum) (diceSides * highBoundSum)
  where
    (m,n) = dimensions p
    diceSides = realToFrac $ m * n
    lowBoundSum = log (diceSides + 1)
    highBoundSum = 1 + log(diceSides)
--

estimateAttemptsNeeded2 :: Plane2 m n -> Double
estimateAttemptsNeeded2 p = fromRational $ (toRational diceSides) * (sum <<< fmap (\i -> 1/(toRational i)) $ [1..diceSides])
  where
    (m,n) = dimensions p
    diceSides = m * n
