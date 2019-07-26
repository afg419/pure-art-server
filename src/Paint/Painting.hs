module Paint.Painting where

import Paint.Graph
import PointGen
import Import

type Painting2 (a :: Asset) (m :: Nat) (n :: Nat) = Graph2 (Locale a) m n
