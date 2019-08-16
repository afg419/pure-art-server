{-# LANGUAGE RecordWildCards #-}

module Paint.Graph where

import Import hiding (elem, filter, map, head, find, group, sort, sum, sortOn)
import Data.List
import Data.Bifoldable

data Edge v = Edge { eSrc :: v, eTgt :: v }
deriving instance Show v => Show (Edge v)
deriving instance Eq v => Eq (Edge v)

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

data Graph v  = Graph { edges :: [Edge v] }
instance Eq v => Semigroup (Graph v) where
  (Graph e1) <> (Graph e2) = Graph <<< rmdups <<$ e1 <> e2
instance Eq v => Monoid (Graph v) where
  mempty = Graph []
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

data BranchCounter v = BranchCounter { val :: v, subBranches :: Integer, subTrees :: Integer }
deriving instance Show v => Show (BranchCounter v)

withBranchCounter :: StarTree v -> StarTree ( BranchCounter v )
withBranchCounter (StarTree v []) = StarTree (BranchCounter v 0 0) []
withBranchCounter (StarTree v sts) = StarTree (BranchCounter v subBranchCount subTreeCount) nextIteration
  where
    nextIteration = fmap withBranchCounter sts
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
