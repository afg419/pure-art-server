{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Daemons.CanvasGeneration where

import PointGen
import Model
import Effects.Paintings
import Control.Monad.Trans.Except
import Import hiding (undefined, sum, filter, elem, print)
import Foundation
import UnliftIO.Concurrent (threadDelay)
import Effects.Interpreters
import Effects.Common

approximateVerticesLoop :: App -> IO ()
approximateVerticesLoop app = forever $ do
  print "running approximation"
  _ <- liftEffectful @PsqlDB app $ do
    paintingEntities <- retrieveParitallyApproximatedPaintings

    for paintingEntities $ \paintingE -> do
      withContext (debug "paintingE: " paintingE) $ \(scty, sPaintingE) -> do
        approximateVerticesWithLocales scty sPaintingE 1000

  threadDelay $ 1 * 1000000

data GenerateLocalesRes =
  LocalesComplete | LocalesImperfect { total :: Natural, imperfect :: Natural, missing :: Natural } --total imerfect missing

approximationReport :: [VertexRecordApproximation a m n] -> GenerateLocalesRes
approximationReport vs = if perfectCount == total
  then LocalesComplete
  else LocalesImperfect total approxCount noCount
  where
    total = fromIntegral <<$ length vs

    (perfects, imperfects) = partition ((== Perfect) <<< getApproximationQuality) vs
    (nones, approxs) = partition ((== None) <<< getApproximationQuality) imperfects

    perfectCount = fromIntegral <<$ length perfects
    approxCount = fromIntegral <<$ length approxs
    noCount = fromIntegral <<$ length nones

approximateVerticesWithLocales ::
  forall a m n r. Paintings r
  => SContext a m n
  -> ValidForContext a m n (Entity PaintingRecord)
  -> Natural
  -> r GenerateLocalesRes
approximateVerticesWithLocales scty sPrec totalTries = do
  let painting = fromSafeCTY <<$ fmap entityVal sPrec
  let sPrid = fmap entityKey sPrec
  vertices <- retrievePaintingVertices (dim scty) sPrid

  let nextTry = fromIntegral (paintingRecordNextPathIndex painting)
  let tries = [nextTry .. nextTry + totalTries - 1]
  let pathsToTry = pathsForIndices tries
  let xpub = paintingRecordXPub painting

  let locales = mapMaybe (deriveLocale scty xpub) pathsToTry

  _ <- for vertices <<$ replaceVertexLocaleIfBetterApproximation locales

  updatePaintingIndex (fromSafeCTY sPrid) (nextTry + totalTries)

  updatedVertices <- retrievePaintingVertices (dim scty) sPrid

  let res = approximationReport updatedVertices
  case res of
    LocalesComplete -> do
      markPaintingFullyApproximated <<$ fromSafeCTY sPrid
      pure res
    _ -> pure res

replaceVertexLocaleIfBetterApproximation :: Paintings s => [SLocale a m n] -> VertexRecordApproximation a m n -> s ()
replaceVertexLocaleIfBetterApproximation locales va =
  case debug "here: " va of
    PerfectRecord _ _ -> pure ()
    NoRecord v -> replaceVertexLocale v localeClosestToVertex
    ApproximateRecord v prevL -> if l1Dist va localeClosestToVertex < l1Dist va prevL
      then replaceVertexLocale v localeClosestToVertex
      else pure ()
  where
    localeClosestToVertex = minOn (l1Dist va) locales


-- On average for a fair n-sided dice it takes n * sum_k=1^n 1/k
estimateAttemptsNeededForEntireCanvas :: (Ord a, Floating a) => Plane m n -> Range a
estimateAttemptsNeededForEntireCanvas p = mkRange (diceSides * lowBoundSum) (diceSides * highBoundSum)
  where
    (m,n) = dimensions p
    diceSides = realToFrac $ m * n
    lowBoundSum = log (diceSides + 1)
    highBoundSum = 1 + log(diceSides)
--

estimateAttemptsNeeded2 :: Plane m n -> Double
estimateAttemptsNeeded2 p = fromRational $ (toRational diceSides) * (sum <<< fmap (\i -> 1/(toRational i)) $ [1..diceSides])
  where
    (m,n) = dimensions p
    diceSides = m * n
