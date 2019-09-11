module Paint.Painting where

import Paint.Graph
import PointGen
import Import

type Painting2 (a :: Asset) (m :: Nat) (n :: Nat) = Graph (SLocale a m n)
