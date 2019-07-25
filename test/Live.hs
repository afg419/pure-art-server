{-# LANGUAGE OverloadedStrings #-}

module Live where

import PointGen
import Import hiding (length, catMaybes, id)
import Data.Maybe (fromJust, catMaybes)
import Daemons.CanvasGeneration
import Effects.Interpreters
import Effects.Common
import Effects.CanvasGeneration
import Application

xpubT :: XPub
xpubT = fromJust <<< parseXPub $ "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"

coordinateT :: KnownNats m n => Plane2 m n -> Coordinate2 m n
coordinateT p2 = Coordinate2 { x = 0, y = 0, plane = p2 }

deriveManyAddresses :: SAsset a -> XPub -> Range Integer -> [(DerivationPath, Maybe (Address a))]
deriveManyAddresses s xpub (Range from' to') = fmap (id &&& deriveAddress s xpub) paths
  where
    paths = fmap (mkPath <<< (0:) <<< pure) [from' .. to']

deriveManyAddressesBenchT :: XPub -> Integer -> IO (Integer, UTCTime)
deriveManyAddressesBenchT xpub top = do
  before <- getCurrentTime
  derivations <- pure $ deriveManyAddresses SDOGE xpub (Range 0 top)
  let result = fromIntegral <<< length <<< catMaybes <<< fmap snd $ derivations
  pure (result, before)

deriveManyLocalesBenchT2 :: (KnownNats m n) => XPub -> Integer -> Plane2 m n -> [Locale m n 'DOGE]
deriveManyLocalesBenchT2 xpub top p2 = catMaybes <<< fmap (deriveLocale SDOGE xpub p2) $ (pathsForIndices [0..top])

benchBulkTest :: KnownNats m n => Plane2 m n -> Integer -> IO (Either String GenerateCanvasRes)
benchBulkTest p2 totalRecords = handler $ do
  runEffects (run @PsqlDB) $ do
    sCanvasId <- (fst <<< fromJust <<< eToM) <$> liftEffectful (insertCanvas2 SDOGE xpubT p2)
    generateCanvasLogic xpubT sCanvasId totalRecords
