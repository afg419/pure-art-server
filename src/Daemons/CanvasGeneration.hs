{-# LANGUAGE RecordWildCards #-}

module Daemons.CanvasGeneration where

import PointGen
import Model
import Effects.CanvasGeneration
import Effects.Common
import Data.Singletons
import Data.List ((\\))
import Import hiding (undefined, length, sum, filter, elem, (\\))

data GenerateWholeCanvasRes = GenerateWholeCanvasRes { foundCoordinates :: Integer, totalCoordinates :: Integer } deriving (Eq, Show)

generateWholeCanvasLogic ::
  forall r m n a. (CanvasGeneration r, KnownNats m n, SingI a)
  => XPub
  -> SCanvas2Id a m n
  -> Integer
  -> Effectful (Interpreter r) (Either String GenerateWholeCanvasRes)
generateWholeCanvasLogic xpub scid totalTries = do
  i <- interpret <$> ask
  i $ do
    -- TODO: check origin address for sufficient funds
    mCanvas2 <- getCanvas2 scid
    case mCanvas2 of
      Nothing -> pure $ Left "Canvas not found."
      Just (SCanvas2 (Canvas2{..}), sAsset) -> do
        let nextTry = fromIntegral canvas2NextPathIndex
        let tries = [nextTry .. nextTry + totalTries - 1]
        let locales = catMaybes <<< fmap (deriveLocale sAsset xpub targetPlane) $ (pathsForIndices tries)

        insertPlane2Locales scid locales
        updateCanvas2NextPathIndex scid (nextTry + totalTries)

        found <- fmap (fromIntegral <<< length) <<< getPlane2Locales $ scid
        pure <<< Right $ GenerateWholeCanvasRes found totalCoordinates
  where
    targetPlane = canvas2Plane scid
    (xs, ys) = plane2Dim targetPlane
    totalCoordinates = xs * ys

data GeneratePaintingCanvasRes = GeneratePaintingCanvasRes
  { foundPaintingCoordinates :: Integer
  , totalPaintingCoordinates :: Integer
  } deriving (Eq, Show)

generatePaintingCanvasLogic ::
  forall r m n a. (CanvasGeneration r, KnownNats m n, SingI a)
  => XPub
  -> SCanvas2Id a m n
  -> Painting2 a m n
  -> Integer
  -> Effectful (Interpreter r) (Either String GeneratePaintingCanvasRes)
generatePaintingCanvasLogic xpub scid painting totalTries = do
  i <- interpret <$> ask
  i $ do
    -- TODO: check origin address for sufficient funds
    mCanvas2 <- getCanvas2 scid
    case mCanvas2 of
      Nothing -> pure $ Left "Canvas not found."
      Just (SCanvas2 (Canvas2{..}), sAsset) -> do
        foundLocales <- getPlane2Locales scid

        let nextTry = fromIntegral canvas2NextPathIndex
        let tries = [nextTry .. nextTry + totalTries - 1]
        let locales = catMaybes <<< fmap (deriveLocale sAsset xpub targetPlane) $ (pathsForIndices tries)

        insertPlane2Locales scid locales
        updateCanvas2NextPathIndex scid (nextTry + totalTries1)

        found <- fmap (fromIntegral <<< length) <<< getPlane2Locales $ scid
        pure <<< Right $ GeneratePaintingCanvasRes found totalCoordinates
  where
    targetPlane = canvas2Plane scid
    (xs, ys) = plane2Dim targetPlane
    totalCoordinates = xs * ys


data CoordinateHuntRes a m n = CoordinateHuntRes
  { foundTargetedLocales :: [Locale a m n]
  , foundLocales :: [Locale a m n]
  , remainingTargetedCoordinates :: [Coordinate2 m n]
  }

multiCoordinateHunt :: KnownNats m n => XPub -> SAsset a -> [Coordinate2 m n] -> Range Integer -> CoordinateHuntRes a m n
multiCoordinateHunt xpub sAsset targetCoordinates (Range startIndex endIndex) =
  CoordinateHuntRes allFoundTargetedLocales allFoundLocales allRemainingTargetedCoordinates
  where
    tryPaths = pathsForIndices [startIndex .. endIndex]
    allFoundLocales = mapMaybe (deriveLocale sAsset xpub P2) <<$ tryPaths
    allFoundTargetedLocales = filter ((`elem` targetCoordinates) <<< lCoordinate) allFoundLocales
    allRemainingTargetedCoordinates = targetCoordinates \\ fmap lCoordinate allFoundTargetedLocales

-- On average for a fair n-sided dice it takes n * sum_k=1^n 1/k
estimateAttemptsNeededForEntireCanvas :: (KnownNats m n , Ord a, Floating a) => Plane2 m n -> Range a
estimateAttemptsNeededForEntireCanvas p = mkRange (diceSides * lowBoundSum) (diceSides * highBoundSum)
  where
    (m,n) = plane2Dim p
    diceSides = realToFrac $ m * n
    lowBoundSum = log (diceSides + 1)
    highBoundSum = 1 + log(diceSides)

estimateAttemptsNeeded2 :: KnownNats m n => Plane2 m n -> Double
estimateAttemptsNeeded2 p = fromRational $ (toRational diceSides) * (sum <<< fmap (\i -> 1/(toRational i)) $ [1..diceSides])
  where
    (m,n) = plane2Dim p
    diceSides = m * n
