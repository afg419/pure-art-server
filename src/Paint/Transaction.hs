{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Paint.Transaction where

import qualified Network.Haskoin.Network as H
import PointGen

import Data.Finite
import Paint.Graph
import Import hiding (Vector, index)


newtype TxScaffoldId v = TxScaffoldId Integer
deriving instance Num (TxScaffoldId v)
data TxScaffold v = TxScaffold { txid :: TxScaffoldId v, input :: InputScaffold v, outs :: [v] }
data InputScaffold v where
  InputScaffold :: { prevId :: TxScaffoldId v, vout :: Integer, from :: v } -> InputScaffold v
  InitInputScaffold :: v -> InputScaffold v

mkInputScaffolds :: TxScaffold v -> [InputScaffold v]
mkInputScaffolds (TxScaffold txid' input outs') = fmap toInput indexedOuts
  where
    indexedOuts = withIndices outs'
    toInput (index, out) = InputScaffold txid' index out

withIndices :: [a] -> [(Integer, a)]
withIndices [] = []
withIndices (a:as) = scanl smash (0,a) as
  where
    smash (i,_) nextA = (i+1, nextA)

-- one startree per connected component of original graph
-- toTxScaffold :: v -> [StarTree v] -> [TxScaffold v]
-- toTxScaffold hotV [] = [TxScaffold 0 (InitScaffold hotV) []]
-- toTxScaffold hotV sts = undefined
--   where

toTxScaffold' :: InputScaffold v -> StarTree v -> [TxScaffold v]
toTxScaffold' input (StarTree v []) = []
toTxScaffold' input (StarTree v sts) = (thisScaffold:nextScaffolds)
  where
    thisId = case input of
      InputScaffold prevId _ _ -> prevId + 1
      InitInputScaffold _ -> 0

    thisScaffold = TxScaffold thisId input (fmap node sts)
    theseInputs = zip (mkInputScaffolds thisScaffold) sts

    toTxScaffoldPairs = uncurry toTxScaffold'
    nextScaffolds = theseInputs >>= toTxScaffoldPairs
--

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
