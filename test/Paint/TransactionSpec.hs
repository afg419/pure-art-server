module Paint.TransactionSpec where

import Test.QuickCheck
import Test.QuickCheck.Assertions ((<=?))
import Gen.PointGen
import Gen.Paint
import PointGen
import Import hiding (print, length)
import Data.Maybe
import Paint

-- center preserverd
prop_starTreeToScaffold1 :: IO ()
prop_starTreeToScaffold1 = quickCheck $ do
  star :: Star Integer <- arbitrary
  let starGraph = toGraph star
  let tree = graphToStarTree starGraph

  scaffold = toTxScaffold 10000 [tree]

  pure $ show scaffold === ""

prop_getDepth1 :: IO ()
prop_getDepth1 = quickCheck $ do
  star :: Star Integer <- arbitrary
  let starGraph = toGraph star
  let center = sSrc star
  let (StarTree (BranchCounter v subBs subTs) sts) = withBranchCounter <<< graphToStarTree starGraph <<$ center

  let firstAssert = subBs === (fromIntegral <<< length <<$ sts)
  let secondAssert = all (== 0) <<< fmap (subBranches <<< node) <<$ sts
  let thirdAssert = subTs === 1
  pure $ firstAssert .&&. secondAssert .&&. thirdAssert

prop_getsConnectedComponents1 :: IO ()
prop_getsConnectedComponents1 = quickCheck $ do
  c1 :: Star Integer <- arbitrary
  pure $ 1 === (length <<< connectedComponents [] <<$ (toGraph c1))
