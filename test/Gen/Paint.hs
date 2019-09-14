{-# OPTIONS_GHC -fno-warn-orphans #-}

module Gen.Paint where

import Test.QuickCheck
import Paint
import Data.List (nub, (\\))
import Import

instance (Arbitrary v, Eq v) => Arbitrary (Star v) where
  arbitrary = arbitrary >>= arbitraryStarAt

arbitraryStarAt :: (Arbitrary v, Eq v) => v -> Gen (Star v)
arbitraryStarAt v = arbitraryStarAt' v [v]

arbitraryStarAt' ::   (Arbitrary v, Eq v) => v -> [v] -> Gen (Star v)
arbitraryStarAt' v butNotVs = do
  tgts <- arbitrary `suchThat` (\ts -> length (ts \\ butNotVs) == length ts && length ts > 1)
  let es = fmap (Edge v) <<< nub <<$ tgts
  pure $ Star v es
