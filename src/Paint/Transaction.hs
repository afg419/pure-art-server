{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Paint.Transaction where

-- import qualified Network.Haskoin.Network as H
import PointGen

import Paint.Graph
import Import hiding (Vector, index)

mkScaffoldId :: Text -> TxScaffoldId v
mkScaffoldId = TxScaffoldId <<< (`div` embarrasinglyLargeNumber) <<< hashIntoInteger

newtype TxScaffoldId v = TxScaffoldId Integer
instance Eq (TxScaffoldId v) where
  (TxScaffoldId v1) == (TxScaffoldId v2) = v1 == v2

instance Show (TxScaffoldId v) where
  show (TxScaffoldId v1) = show v1

deriving instance Num (TxScaffoldId v)
data TxScaffold v = TxScaffold { txid :: TxScaffoldId v, input :: InputScaffold v, outs :: [v] }
data InputScaffold v where
  InputScaffold :: { prevId :: TxScaffoldId v, vout :: Integer, from :: v } -> InputScaffold v
  InitInputScaffold :: v -> InputScaffold v
deriving instance Eq v => Eq (InputScaffold v)

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
toTxScaffold hotV sts = sts >>= toTxScaffold' initInput
  where
    initInput = InitInputScaffold hotV

toTxScaffold' :: Show v => InputScaffold v -> StarTree v -> [TxScaffold v]
toTxScaffold' _ (StarTree _ []) = []
toTxScaffold' input' (StarTree v sts) = (thisScaffold:nextScaffolds)
  where
    thisId = mkScaffoldId <<< pack <<< (show v <>) <<< show <<< fmap (show <<< node) <<$ sts

    thisScaffold = TxScaffold thisId input' (fmap node sts)
    theseInputs = zip (mkInputScaffolds thisScaffold) sts

    toTxScaffoldPairs = uncurry toTxScaffold'
    nextScaffolds = theseInputs >>= toTxScaffoldPairs
--
embarrasinglyLargeNumber :: Integer
embarrasinglyLargeNumber = 1000000000000000000000000000000000000000000000000
--
-- toLocales :: TxScaffold a k m n -> Vector k (Locale a m n)
-- toLocales (TxScaffold _ v) = v
-- toLocales (InitTxScaffold v) = v
--
-- toAddresses :: TxScaffold a k m n -> Vector k (Address a)
-- toAddresses = fmap lAddress <<< toLocales
--
-- fromLocale :: TxScaffold a k m n -> Maybe (Locale a m n)
-- fromLocale (TxScaffold o _) = Just <<< fromLocale' $ o
-- fromLocale (InitTxScaffold _) = Nothing
--
-- fromAddress :: TxScaffold a k m n -> Maybe (Address a)
-- fromAddress (TxScaffold o _) = Just <<< fromAddress' $ o
-- fromAddress (InitTxScaffold _) = Nothing
--
-- data PreOutpoint (a :: Asset) (m :: Nat) (n :: Nat) where
--   PreOutpoint :: TxScaffold a k m n -> Finite k -> PreOutpoint a m n
--
-- fromLocale' :: PreOutpoint a m n -> Locale a m n
-- fromLocale' (PreOutpoint pre k) = toLocales pre `index` k
--
-- fromAddress' :: PreOutpoint a m n -> Address a
-- fromAddress' = lAddress <<< fromLocale'
