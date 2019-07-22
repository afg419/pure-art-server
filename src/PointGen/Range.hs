module PointGen.Range where

data Range a = Range a a

mkRange :: Ord a => a -> a -> Range a
mkRange r1 r2 = Range (min r1 r2) (max r1 r2)

diameter :: Num a => Range a -> a
diameter (Range bot top) = top - bot

inRange :: Ord a => a -> Range a -> Bool
inRange  a (Range bot top) = bot <= a && a <= top
