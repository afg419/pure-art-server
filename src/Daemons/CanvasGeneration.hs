{-# LANGUAGE RecordWildCards #-}

module Daemons.CanvasGeneration where

import PointGen
import Model
import Effects.CanvasGeneration
import Import hiding (undefined, sum, filter, elem)

data GeneratePubKeysRes = GeneratePubKeysRes
  { totalAddresses :: Natural
  } deriving (Eq, Show)

generatePubKeys ::
  forall r. CanvasGeneration r
  => XPub
  -> Natural
  -> r (Maybe GeneratePubKeysRes)
generatePubKeys xpub totalTries = do
  -- undefined
  -- TODO: check origin address for sufficient funds
  mGenerator <- fmap (fmap (entityVal &&& entityKey)) <<< getPublicKeyGenerator <<$ xpub
  case mGenerator of
    Nothing -> pure Nothing
    Just (PublicKeyGenerator {..}, pkgenId) -> do
      let nextTry = fromIntegral publicKeyGeneratorNextPathIndex
      let tries = [nextTry .. nextTry + totalTries - 1]
      let pubsWPaths = (pathsForIndices tries)
            $>> fmap (id &&& mkPublicKey publicKeyGeneratorXpub)
            >>> fmap sequenceA
            >>> catMaybes
            >>> fmap toInsertFormat

      -- let pubsWPaths = fmap toInsertFormat <<< catMaybes <<$ fmap (sequenceA <<< (id &&& mkPublicKey publicKeyGeneratorXpub)) (pathsForIndices tries)

      insertPublicKeys pkgenId pubsWPaths
      updateGeneratorIndex pkgenId (nextTry + totalTries)

      found <- fmap (fromIntegral <<< length) <<$ getPublicKeys pkgenId
      pure <<< Just <<$ GeneratePubKeysRes found

  where
    toInsertFormat (path, pub) = (pub, path)


-- generateWholeCanvasLogic :: CanvasGeneration r => SCanvas2 a m n -> Natural -> r (Either Text GenerateWholeCanvasRes)
-- generateWholeCanvasLogic (SCanvas2 (Canvas2 {..})) totalTries = do
--

--
-- data GeneratePaintingCanvasRes = GeneratePaintingCanvasRes
--   { foundPaintingCoordinates :: Integer
--   , totalPaintingCoordinates :: Integer
--   } deriving (Eq, Show)
--
-- -- TODO: this isn't right.
-- generatePaintingCanvasLogic ::
--   forall r m n a. (CanvasGeneration r, KnownNats m n, SingI a)
--   => SAsset a
--   -> XPub
--   -> SCanvas2Id a m n
--   -> Painting2 a m n
--   -> Integer
--   -> Effectful (Interpreter r) (Either String GeneratePaintingCanvasRes)
-- generatePaintingCanvasLogic sAsset xpub scid@(SCanvas2Id cid) _ totalTries = do
--   i <- interpret <$> ask
--   i $ do
--     -- TODO: check origin address for sufficient funds
--     mCanvas2 <- sGetCanvas2 scid
--     case mCanvas2 of
--       Nothing -> pure $ Left "Canvas not found."
--       Just (SCanvas2 (Canvas2{..})) -> do
--         -- foundLocales <- getPlane2Locales scid
--
--         let nextTry = fromIntegral canvas2NextPathIndex
--         let tries = [nextTry .. nextTry + totalTries - 1]
--         let locales = catMaybes <<< fmap (deriveLocale sAsset xpub targetPlane) $ (pathsForIndices tries)
--
--         insertPlane2Locales sAsset scid locales
--         updateCanvas2NextPathIndex scid (nextTry + totalTries)
--
--         found :: [SLocale a m n] <- getPlane2Locales cid
--         pure <<< Right $ GeneratePaintingCanvasRes (fromIntegral <<< length <<$ found) totalCoordinates
--   where
--     targetPlane = canvas2Plane scid
--     (xs, ys) = plane2Dim targetPlane
--     totalCoordinates = xs * ys
--
--
-- data CoordinateHuntRes a m n = CoordinateHuntRes
--   { foundTargetedLocales :: [SLocale a m n]
--   , foundLocales :: [SLocale a m n]
--   , remainingTargetedCoordinates :: [SCoordinate2 m n]
--   }
--
-- multiCoordinateHunt :: XPub -> SAsset a -> [SCoordinate2 m n] -> Range Integer -> CoordinateHuntRes a m n
-- multiCoordinateHunt xpub sAsset targetCoordinates (Range startIndex endIndex) =
--   CoordinateHuntRes allFoundTargetedLocales allFoundLocales allRemainingTargetedCoordinates
--   where
--     tryPaths = pathsForIndices [startIndex .. endIndex]
--     allFoundLocales = mapMaybe (deriveLocale sAsset xpub P2) <<$ tryPaths
--     allFoundTargetedLocales = filter ((`elem` targetCoordinates) <<< lCoordinate) allFoundLocales
--     allRemainingTargetedCoordinates = targetCoordinates \\ fmap lCoordinate allFoundTargetedLocales
--

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
