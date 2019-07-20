{-# LANGUAGE RecordWildCards #-}

module Daemons.CanvasGeneration where

import PointGen
import Model
import Effects.CanvasGeneration
import Effects.Common
import Data.Either
import Data.Singletons
import Import hiding (undefined, length)

data GenerateCanvasRes = GenerateCanvasRes { foundCoordinates :: Integer, totalCoordinates :: Integer }

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
        _ <- traverse (deriveAndGinsert scid sAsset xpub targetPlane) (tryPaths tries)
        found <- fmap (fromIntegral <<< length <<< fromRight []) <<< getPlane2Locales $ scid
        pure <<< Right $ GenerateCanvasRes found totalCoordinates
  where
    targetPlane = canvas2Plane scid
    (xs, ys) = plane2Dim targetPlane
    totalCoordinates = xs * ys
    tryPaths t = fmap (mkPath <<< (0:) <<< pure) t


deriveAndGinsert :: (KnownNats m n , CanvasGeneration r) => SCanvas2Id m n a -> SAsset a -> XPub -> Plane2 m n -> DerivationPath -> r ()
deriveAndGinsert scid s x p d = case deriveLocale s x p d of
  Nothing -> pure ()
  Just locale -> ginsertPlane2Locale scid locale >> pure ()
