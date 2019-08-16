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
  pure $ (v === center) .&&. (fmap (Edge center <<< stSrc) sts) === rays star

prop_getDepth1 :: IO ()
prop_getDepth1 = quickCheck $ do
  star :: Star Integer <- arbitrary
  let starGraph = toGraph star
  let center = sSrc star
  let (StarTree (BranchCounter _ subBs subTs) sts) = withBranchCounter <<< graphToStarTree starGraph <<$ center

  let firstAssert = subBs === (fromIntegral <<< length <<$ sts)
  let secondAssert = all (== 0) <<< fmap (subBranches <<< stSrc) <<$ sts
  let thirdAssert = subTs === 1
  pure $ firstAssert .&&. secondAssert .&&. thirdAssert

prop_getsConnectedComponents1 :: IO ()
prop_getsConnectedComponents1 = quickCheck $ do
  c1 :: Star Integer <- arbitrary
  pure $ 1 === (length <<< connectedComponents [] <<$ (toGraph c1))

prop_getsConnectedComponents2 :: IO ()
prop_getsConnectedComponents2 = quickCheck $ do
  c1 :: Star Integer <- arbitrary
  nextCenter <- arbitrary `suchThat` (not <<< inStar c1)
  c2 <- arbitraryStarAt' nextCenter (sSrc c1:rayTgts c1)

  pure $ 2 === (length <<< connectedComponents [] <<$ (toGraph c1 <> toGraph c2))
