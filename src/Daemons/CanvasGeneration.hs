{-# LANGUAGE RecordWildCards #-}

module Daemons.CanvasGeneration where

import PointGen
import Model
import Effects.CanvasGeneration
import Effects.Common
import Data.Singletons
import Import hiding (undefined, length, sum)

data GenerateCanvasRes = GenerateCanvasRes { foundCoordinates :: Integer, totalCoordinates :: Integer } deriving (Eq, Show)

generateCanvasLogic :: forall r m n a. (CanvasGeneration r, KnownNats m n, SingI a) => XPub -> SCanvas2Id m n a -> Integer -> Effectful (Interpreter r) (Either String GenerateCanvasRes)
generateCanvasLogic xpub scid totalTries = do
  i <- interpret <$> ask
  i $ do
    mCanvas2 <- getCanvas2 scid
    case mCanvas2 of
      Nothing -> pure $ Left "Canvas not found."
      Just (SCanvas2 (Canvas2{..}), sAsset) -> do
        let nextTry = fromIntegral canvas2NextPathIndex
        let tries = [nextTry .. nextTry + totalTries]
        let locales = catMaybes <<< fmap (deriveLocale sAsset xpub targetPlane) $ (pathsForIndices tries)

        insertPlane2Locales scid locales
        updateCanvas2NextPathIndex scid (nextTry + totalTries + 1)
        
        found <- fmap (fromIntegral <<< length) <<< getPlane2Locales $ scid
        pure <<< Right $ GenerateCanvasRes found totalCoordinates
  where
    targetPlane = canvas2Plane scid
    (xs, ys) = plane2Dim targetPlane
    totalCoordinates = xs * ys

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
