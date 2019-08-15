{-# LANGUAGE GADTs #-}

module Paint.Transaction where

import qualified Network.Haskoin.Network as H
import PointGen

import Data.Vector.Sized
import Data.Finite
import Import hiding (Vector, index)



-- data TxScaffold (a :: Asset) (m :: Nat) (n :: Nat) = TxScaffold { id :: TxScaffoldId, input :: OutpointScaffold v, outs :: [v] }
--
-- data OutpointScaffold v = OutpointScaffold { prevId :: TxScaffoldId, vout :: Integer }

-- data TxScaffold (a :: Asset) (k :: Nat) (m :: Nat) (n :: Nat) where
--   TxScaffold :: PreOutpoint a m n -> Vector k (Locale a m n) -> TxScaffold a k m n
--   InitTxScaffold :: Vector k (Locale a m n) -> TxScaffold a k m n
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
