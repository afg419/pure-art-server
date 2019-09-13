{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

module Paint.Graph where

import Import hiding (elem, filter, map, find, group, sort, sum, sortOn)
import Data.List hiding (last)
import Data.Bifoldable
import Data.Aeson
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM

data Edge v = Edge { eSrc :: v, eTgt :: v } deriving (Functor, Foldable, Traversable)
deriving instance Show v => Show (Edge v)
instance Eq v => Eq (Edge v) where -- for now we don't distinguish between sources and targets
  Edge s1 t1 == Edge s2 t2 = ((s1 == s2) || (s1 == t2)) && ((t1 == s2) || (t1 == t2))

instance FromJSON v => FromJSON (Edge v) where
  parseJSON = withArray "edge" $ \valueVector -> do
    if V.length valueVector /= 2
      then fail "Could not parse as edge, length not 2"
      else do
        src <- parseJSON <<$ V.head valueVector
        tgt <- parseJSON <<$ V.last valueVector
        pure $ Edge src tgt

vertices :: Eq v => [Edge v] -> [v]
vertices = const True $>> verticesSuchThat

verticesSuchThat :: Eq v => (v -> Bool) -> [Edge v] -> [v]
verticesSuchThat st es = es
  >>= ( edgeVertices >>> biList >>> filter st )
  $>> nub

edgeVertices :: Edge v -> (v, v)
edgeVertices = eSrc &&& eTgt

data Star v = Star { sSrc :: v, rays :: [Edge v] }
deriving instance Show v => Show (Star v)

toGraph :: Star v -> Graph v
toGraph = rays >>> Graph

inStar :: Eq v => Star v -> v -> Bool
inStar s v = isJust $ find (==v) (sSrc s : rayTgts s)

rayTgts :: Eq v => Star v -> [v]
rayTgts s = verticesSuchThat (/= v) es
  where
    v = sSrc s
    es = rays s

vertexEdges :: Eq v => [Edge v] -> v -> Star v
vertexEdges es v = Star v rays
  where
    rays = es $>> mapMaybe (v `inEdgeM`)

starSize :: Eq v => Star v -> Integer
starSize = rayTgts >>> length >>> fromIntegral

inEdge :: Eq v => v -> Edge v -> Bool
inEdge v e = v `elem` (biList <<< edgeVertices <<$ e)

inEdgeM :: Eq v => v -> Edge v -> Maybe (Edge v)
inEdgeM v e = if (v `inEdge` e)
  then Just e
  else Nothing

data Graph v  = Graph { edges :: [Edge v] } deriving (Functor, Foldable, Traversable)
instance Eq v => Semigroup (Graph v) where
  (Graph e1) <> (Graph e2) = Graph <<< rmdups <<$ e1 <> e2
instance Eq v => Monoid (Graph v) where
  mempty = Graph []

instance FromJSON v => FromJSON (Graph v) where
  parseJSON = withArray "edge list" $ \edgeVector -> do
    edges <- traverse parseJSON edgeVector
    pure <<< Graph <<< toList <<$ edges

singletonG :: v -> Graph v
singletonG v = Graph <<$ [Edge v v]

deriving instance Show v => Show (Graph v)
deriving instance Eq v => Eq (Graph v)
mkGraph2 :: Eq v => [Edge v] -> Graph v
mkGraph2 = nub >>> Graph

verticesG :: Eq v => Graph v -> [v]
verticesG = edges >>> vertices

vertexInGraph :: Eq v => Graph v ->  v -> Bool
vertexInGraph g v = v `elem` verticesG g

vertexEdgesG :: Eq v => Graph v -> v -> Star v
vertexEdgesG g v = vertexEdges (edges g) v

connectedComponents :: (Show v, Eq v) => [Graph v] -> Graph v -> [Graph v]
connectedComponents _ (Graph []) = []
connectedComponents visitedComponents g =
  case remainingVertices of
    [] -> []
    (next:_) -> let nextComponent = componentFor visitedVertices g next in
      nextComponent : (connectedComponents (nextComponent : visitedComponents) g)
  where
    visitedVertices = join <<< fmap verticesG <<$ visitedComponents
    remainingVertices = verticesG g \\ visitedVertices

componentFor :: Eq v =>  [v] -> Graph v -> v -> Graph v
componentFor _ (Graph []) v = singletonG v
componentFor visited (Graph es) v = mconcat <<$ toGraph vStar : recurse1
  where
    vStar = vertexEdges es v
    remainingEdges = es \\ (rays vStar)

    adjacentVertices = rayTgts vStar \\ visited
    visited' = adjacentVertices <> visited
    recurse1 = fmap (componentFor visited' (Graph remainingEdges)) adjacentVertices

data StarTree v = StarTree v [StarTree v]
starTreeShallowSize :: StarTree v -> Integer
starTreeShallowSize = branches >>> length >>> fromIntegral

instance Eq v => Eq (StarTree v) where
  (StarTree v sts) == (StarTree v2 sts2) = v == v2 && sts == sts2
instance Show v => Show (StarTree v) where
  show (StarTree v sts) = show v <> showBranches
    where
      showBranches = if length sts == 0 then ""
        else "-<" <> show sts <> ">-"
stSrc :: StarTree v -> v
stSrc (StarTree v _) = v
branches :: StarTree v -> [StarTree v]
branches (StarTree _ sts) = sts


-- Fee of transactions = fee_in for 1 input + fee_out * total outputs.
-- amount needed for tx = amount needed for each out + fee_in for 1 input + fee_out * total outputs.

data Funds = Zero | Fin Natural | Outp Natural | Inp Natural | Plus Funds Funds
instance Show Funds where
  show (Fin n) = show n <> "F"
  show (Outp n) = show n <> "O"
  show (Inp n) = show n <> "I"
  show (Plus a b) = show a <> " + " <> show b

instance Semigroup Funds where
  (Fin i) <> (Fin j) = Fin $ i + j
  (Outp i) <> (Outp j) = Outp $ i + j
  (Inp i) <> (Inp j) = Inp $ i + j
  Zero <> a = a
  a <> Zero = a
  a <> b = Plus a b

instance Eq Funds where
  Fin i == Fin j = i == j
  Outp i == Outp j = i == j
  Inp i == Inp j = i == j
  Zero == Zero = True
  Plus a b = c ==
  

instance ToJSON Funds where
  toJSON f = String $ tshow f

instance Monoid Funds where
  mempty = Zero

data BranchCounter v = BranchCounter { node :: v, funds :: Funds }
deriving instance Show v => Show (BranchCounter v)
deriving instance Functor BranchCounter
instance ToJSON v => ToJSON (BranchCounter v) where
  toJSON (BranchCounter node funds) = case toJSON node of
    Object o -> Object $ appendCounterKeys o
    _ -> object
      [ "funds" .= toJSON funds
      , "node" .= toJSON node]
    where
      appendCounterKeys hm = HM.insert "funds" (toJSON funds) <<$ hm

withBranchCounter :: StarTree v -> StarTree (BranchCounter v)
withBranchCounter (StarTree v []) = StarTree (BranchCounter v finalFunds) []
  where
    finalFunds = Inp 1 <> Fin 1
withBranchCounter (StarTree v sts) = StarTree (BranchCounter v subBranchCount subTreeCount) nextIteration
  where
    nextIteration = fmap withBranchCounter sts
    fundsForNextIteration = mconcat <> fmap funds nextIteration


    branchCount = fromIntegral <<$ length sts
    subBranchCount = branchCount + (sum <<< fmap (subBranches <<< stSrc) <<$ nextIteration)
    subTreeCount = 1 + (sum <<< fmap (subTrees <<< stSrc) <<$ nextIteration)

-- a disconnected graph will only return star tree for component containing v
graphToStarTree :: (Show v, Eq v)  => Graph v -> v -> StarTree v
graphToStarTree g v = snd <<$ graphToStarTree' g v

graphToStarTree' :: (Show v, Eq v) => Graph v -> v -> ([Edge v], StarTree v)
graphToStarTree' g v
  | not (vertexInGraph g v) = ([], StarTree v [])
  | otherwise = ret
  where
    nextSrcStar = vertexEdgesG g v
    nextVertices = sortOn (((-1) *) <<< starSize <<< vertexEdgesG g) (rayTgts nextSrcStar)

    ret = case nextVertices of
      [] -> ([], StarTree v [])
      vs -> let (_:nextTrees) = scanl mySmash (rays nextSrcStar, StarTree v []) vs in
        (fromMaybe [] (lastMay $ fmap fst nextTrees) , StarTree v (fmap snd nextTrees))

    mySmash (usedEdges, _) vertex =
      first (<> usedEdges) $ graphToStarTree' (Graph $ (edges g \\ usedEdges)) vertex

leaves :: StarTree v -> [v]
leaves (StarTree v []) = [v]
leaves (StarTree _ es) = es >>= leaves

subbranches :: StarTree v -> [v]
subbranches (StarTree v []) = [v]
subbranches (StarTree v es) = v : (es >>= leaves)
