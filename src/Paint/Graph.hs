{-# LANGUAGE RecordWildCards #-}

module Paint.Graph where

import Import hiding (elem, filter, length)
import Data.List
import Data.Bifoldable

data Edge2 v (m :: Nat) (n :: Nat) = Edge2 { eSrc :: v m n, eTgt :: v m n }
deriving instance Show (v m n) => Show (Edge2 v m n)
deriving instance Eq (v m n) => Eq (Edge2 v m n)

vertices :: Eq (v m n) => [Edge2 v m n] -> [v m n]
vertices = const True $>> verticesSuchThat

verticesSuchThat :: Eq (v m n) => (v m n -> Bool) -> [Edge2 v m n] -> [v m n]
verticesSuchThat st es = es
  >>= ( edgeVertices >>> biList >>> filter st )
  $>> nub

edgeVertices :: Edge2 v m n -> (v m n, v m n)
edgeVertices = eSrc &&& eTgt

data Star2 v (m :: Nat) (n :: Nat) = Star2 { sSrc :: v m n, rayTgts :: [v m n] }

vertexEdges :: Eq (v m n) => [Edge2 v m n] -> v m n -> Star2 v m n
vertexEdges es v = Star2 v tgts
  where
    tgts = es $>> mapMaybe (v `inEdgeM`) >>> verticesSuchThat (/= v)
starSize :: Star2 v m n -> Integer
starSize = rayTgts >>> length >>> fromIntegral

-- starIntersection :: Star2 v m n -> Star2 v m n -> Maybe (Edge2 v m n)
-- starIntersection

inEdge :: Eq (v m n) => v m n -> Edge2 v m n -> Bool
inEdge v e = v `elem` (biList <<< edgeVertices <<$ e)

inEdgeM :: Eq (v m n) => v m n -> Edge2 v m n -> Maybe (Edge2 v m n)
inEdgeM v e = if (v `inEdge` e)
  then Just e
  else Nothing

data Graph2 v (m :: Nat) (n :: Nat) = Graph2 { edges :: [Edge2 v m n] }
deriving instance Show (v m n) => Show (Graph2 v m n)
deriving instance Eq (v m n) => Eq (Graph2 v m n)
mkGraph2 :: Eq (v m n) => [Edge2 v m n] -> Graph2 v m n
mkGraph2 = nub >>> Graph2

verticesG2 :: Eq (v m n) => Graph2 v m n -> [v m n]
verticesG2 = edges >>> vertices

vertexEdgesG2 :: Eq (v m n) => Graph2 v m n -> v m n -> Star2 v m n
vertexEdgesG2 g v = vertexEdges (edges g) v
