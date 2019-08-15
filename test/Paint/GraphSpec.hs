module Paint.GraphSpec where

import Test.QuickCheck
import Gen.Paint
import Import
import Paint

-- center preserverd
prop_starGraphToSpanningTree1 :: IO ()
prop_starGraphToSpanningTree1 = quickCheck $ do
  star :: Star Integer <- arbitrary
  let starGraph = toGraph star
  let center = sSrc star
  let (StarTree v sts) = graphToStarTree starGraph center
  pure $ (v === center) .&&. (fmap (Edge center <<< node) sts) === rays star

prop_getDepth1 :: IO ()
prop_getDepth1 = quickCheck $ do
  star :: Star Integer <- arbitrary
  let starGraph = toGraph star
  let center = sSrc star
  let (StarTree (BranchCounter _ subBs subTs) sts) = withBranchCounter <<< graphToStarTree starGraph <<$ center

  let firstAssert = subBs === (fromIntegral <<< length <<$ sts)
  let secondAssert = all (== 0) <<< fmap (subBranches <<< node) <<$ sts
  let thirdAssert = subTs === 1
  pure $ firstAssert .&&. secondAssert .&&. thirdAssert

prop_getsConnectedComponents1 :: IO ()
prop_getsConnectedComponents1 = quickCheck $ do
  c1 :: Star Integer <- arbitrary
  pure $ 1 === (length <<< connectedComponents [] <<$ (toGraph c1))
