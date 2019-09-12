{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Paint.Transaction where

import PointGen

import Paint.Graph
import Import hiding (Vector, index)

graphToTxScaffold :: (Show v, Eq v) => v -> Graph v -> [TxScaffold (BranchCounter v)]
graphToTxScaffold hotV g = toTxScaffold initBranchCounter counterStarTrees
  where
    initBranchCounter = BranchInitCounter hotV
    components = connectedComponents [] g
    pointedComponents = fmap (id &&& (head <<< verticesG)) components
    counterStarTrees = fmap (withBranchCounter <<< uncurry graphToStarTree) pointedComponents

mkScaffoldId :: Text -> TxScaffoldId v
mkScaffoldId = TxScaffoldId <<< (`div` embarrasinglyLargeNumber) <<< hashToNatural

newtype TxScaffoldId v = TxScaffoldId Natural
instance Eq (TxScaffoldId v) where
  (TxScaffoldId v1) == (TxScaffoldId v2) = v1 == v2

instance Show v => Show (TxScaffoldId v) where
  show (TxScaffoldId v1) = show v1

deriving instance Num (TxScaffoldId v)
data TxScaffold v = TxScaffold { txid :: TxScaffoldId v, input :: InputScaffold v, outs :: [v] }
deriving instance Show v => Show (TxScaffold v)

data InputScaffold v where
  InputScaffold :: { prevId :: TxScaffoldId v, vout :: Integer, from :: v } -> InputScaffold v
  InitInputScaffold :: v -> InputScaffold v
deriving instance Eq v => Eq (InputScaffold v)
deriving instance Show v => Show (InputScaffold v)

mkInputScaffolds :: TxScaffold v -> [InputScaffold v]
mkInputScaffolds (TxScaffold txid' _ outs') = fmap toInput indexedOuts
  where
    indexedOuts = withIndices outs'
    toInput (index, out) = InputScaffold txid' index out

withIndices :: [a] -> [(Integer, a)]
withIndices [] = []
withIndices (a:as) = scanl smash (0,a) as
  where
    smash (i,_) nextA = (i+1, nextA)

-- one startree per connected component of original graph
toTxScaffold :: Show v => v -> [StarTree v] -> [TxScaffold v]
toTxScaffold _ [] = []
toTxScaffold hotV sts = toTxScaffold' initInput combinedSTree
  where
    combinedSTree = StarTree hotV sts
    initInput = InitInputScaffold hotV

toTxScaffold' :: Show v => InputScaffold v -> StarTree v -> [TxScaffold v]
toTxScaffold' _ (StarTree _ []) = []
toTxScaffold' input' (StarTree v sts) = (thisScaffold:nextScaffolds)
  where
    thisId = mkScaffoldId <<< pack <<< (show v <>) <<< show <<< fmap (show <<< stSrc) <<$ sts

    thisScaffold = TxScaffold thisId input' (fmap stSrc sts)
    theseInputs = zip (mkInputScaffolds thisScaffold) sts

    toTxScaffoldPairs = uncurry toTxScaffold'
    nextScaffolds = theseInputs >>= toTxScaffoldPairs
--
embarrasinglyLargeNumber :: Natural
embarrasinglyLargeNumber = 1000000000000000000000000000000000000000000000000
