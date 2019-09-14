{-# LANGUAGE OverloadedStrings #-}

module Live where

import PointGen
import Import
import Data.Maybe (fromJust)
-- import Daemons.CanvasGeneration
-- import Effects.Interpreters
-- import Effects.Common
-- import Effects.CanvasGeneration
import Model
import Paint
-- import Application

xpubT :: XPub
xpubT = fromJust <<< parseXPub <<$ "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"

getCanvas2AtCurrent :: IO Canvas2
getCanvas2AtCurrent = do
  time <- getCurrentTime
  pure $ Canvas2 xpubT 100 150 DOGE 0 time time

coordinates :: [Coordinate2]
coordinates = [(0,0), (50, 20), (200, 300)]

edgesSafe :: Graph Coordinate2
edgesSafe = Graph [Edge (0,0) (50,20), Edge (0,0) (3,4)]
edgesUnsafe :: Graph Coordinate2
edgesUnsafe = Graph [Edge (0,0) (50,20), Edge (0,0) (3,4), Edge (0,0) (200,400)]

-- coordinateT :: KnownNats m n => Plane2 m n -> SCoordinate2 m n
-- coordinateT p2 = SCoordinate2 { x = 0, y = 0, plane = p2 }
--
-- deriveManyAddresses :: SAsset a -> XPub -> Range Integer -> [(DerivationPath, Maybe (Address a))]
-- deriveManyAddresses s xpub (Range from' to') = fmap (id &&& deriveAddress s xpub) paths
--   where
--     paths = fmap (mkPath <<< (0:) <<< pure) [from' .. to']
--
-- deriveManyAddressesBenchT :: XPub -> Integer -> IO (Integer, UTCTime)
-- deriveManyAddressesBenchT xpub top = do
--   before <- getCurrentTime
--   derivations <- pure $ deriveManyAddresses SDOGE xpub (Range 0 top)
--   let result = fromIntegral <<< length <<< catMaybes <<< fmap snd $ derivations
--   pure (result, before)
--
-- deriveManyLocalesBenchT2 :: (KnownNats m n) => XPub -> Integer -> Plane2 m n -> [SLocale 'DOGE m n]
-- deriveManyLocalesBenchT2 xpub top p2 = catMaybes <<< fmap (deriveLocale SDOGE xpub p2) $ (pathsForIndices [0..top])
--
-- benchBulkTest :: KnownNats m n => Plane2 m n -> Integer -> IO (Either String GenerateWholeCanvasRes)
-- benchBulkTest p2 totalRecords = handler $ do
--   runEffects (run @PsqlDB) $ do
--     sCanvasId <- (fromJust <<< eToM) <$> liftEffectful (insertCanvas2 xpubT p2)
--     generateWholeCanvasLogic SDOGE xpubT sCanvasId totalRecords
