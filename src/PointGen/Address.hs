{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module PointGen.Address where

import qualified Crypto.HDTree.Address as Crypto
import Data.Singletons.TH

$(singletons [d|
  data Asset = DOGE
    deriving (Show, Eq)
  |])

data Address ( a:: Asset) where
  DogeA :: Crypto.DogeAddr -> Address 'DOGE
deriving instance Show (Address 'DOGE)
deriving instance Eq (Address 'DOGE)
