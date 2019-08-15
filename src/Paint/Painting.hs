module Paint.Painting where

import Paint.Graph
import PointGen
import Import

type Painting2 (a :: Asset) (m :: Nat) (n :: Nat) = Graph (Locale a m n)
