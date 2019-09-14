module Paint.TransactionSpec where

import Test.QuickCheck
import Gen.Paint
import Import hiding (print, from)
import Paint
import Data.List ((\\))

prop_starTreeToScaffold1 :: IO ()
prop_starTreeToScaffold1 = quickCheck $ do
  star :: Star String <- arbitrary
  let starGraph = toGraph star
  let tree = graphToStarTree starGraph (sSrc star)

  let [scaffold] = toTxScaffold "hotAddress" [tree]

  pure $ (outs scaffold) === (rayTgts star)

prop_starTreeToScaffold2_DisconnectedTrees :: IO ()
prop_starTreeToScaffold2_DisconnectedTrees = quickCheck $ do
  c1 :: Star Integer <- arbitrary
  let t1 = graphToStarTree (toGraph c1) (sSrc c1)

  nextCenter <- arbitrary `suchThat` (not <<< inStar c1)
  c2 <- arbitraryStarAt' nextCenter (sSrc c1:rayTgts c1)
  let t2 = graphToStarTree (toGraph c2) (sSrc c2)

  let [hotScaffold, c1Scaffold, c2Scaffold] = toTxScaffold 1000 [t1, t2]

  pure $   outs hotScaffold === [sSrc c1, sSrc c2]
      .&&. outs c1Scaffold === rayTgts c1
      .&&. outs c2Scaffold === rayTgts c2
      .&&. (from $ input c1Scaffold) === sSrc c1
      .&&. (from $ input c2Scaffold) === sSrc c2

prop_starTreeToScaffold3_LengthenedTree :: IO ()
prop_starTreeToScaffold3_LengthenedTree = quickCheck $ do
  c1 :: Star Integer <- arbitrary `suchThat` \s -> length (rayTgts s) > 0

  center2 <- elements (rayTgts c1)
  c2 <- arbitraryStarAt center2
  let t = graphToStarTree (toGraph c2 <> toGraph c1) (sSrc c1)

  let gs = toTxScaffold 1000 [t]

  pure $ length gs === 3

prop_starTreeToScaffold4_Cyclic :: IO ()
prop_starTreeToScaffold4_Cyclic = quickCheck $ do
  c1 :: Star Integer <- arbitrary `suchThat` \s -> length (rayTgts s) > 1

  center2 <- elements (rayTgts c1)
  c2 <- arbitraryStarAt center2 `suchThat` \s -> length (rayTgts s) > 1

  toConnectBack1 <- elements (rayTgts c1 \\ [center2])
  toConnectBack2 <- elements (rayTgts c2 \\ [sSrc c1])

  let graph = toGraph c2 <> toGraph c1 <> (Graph [Edge toConnectBack1 toConnectBack2])
  let t = graphToStarTree graph (sSrc c1)

  let gs = toTxScaffold 1000 [t]

  pure $ length gs === 3 .||. length gs === 4
