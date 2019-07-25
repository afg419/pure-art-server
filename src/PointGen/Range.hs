module PointGen.Range where

-- ranges represent _closed_ intervals
data Range a = Range a a

instance Show a => Show (Range a) where
  show (Range a1 a2) = "Range[" <> show a1 <> "," <> show a2 <> "]"

mkRange :: Ord a => a -> a -> Range a
mkRange r1 r2 = Range (min r1 r2) (max r1 r2)

diameter :: Num a => Range a -> a
diameter (Range bot top) = top - bot + 1

inRange :: Ord a => a -> Range a -> Bool
inRange  a (Range bot top) = bot <= a && a <= top

low :: Range a -> a
low (Range l _) = l

high :: Range a -> a
high (Range _ h) = h
