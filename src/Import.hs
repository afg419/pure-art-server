module Import
    ( module Import
    ) where

import Import.NoFoundation   as Import hiding (all, length, (\\), zip, id, head, Proxy)
import Control.Category      as Import hiding (id)
import GHC.Read as Import
import GHC.Generics as Import hiding (from, to)
import Data.Singletons.TypeLits as Import
import GHC.Natural as Import
import GHC.Word as Import
import qualified Data.Foldable as F

fgmap :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
fgmap = fmap >>> fmap

minOn :: (F.Foldable t, Ord a) => (b -> a) -> t b -> b
minOn = F.minimumBy <<< comparing

preimage :: Eq b => (a -> b) -> b -> [a] -> [a]
preimage f b = Prelude.filter ((== b) <<< f)

every :: forall a. (Bounded a, Enum a) => [a]
every = fmap toEnum [ minInt .. maxInt ]
  where
    minInt = fromEnum (minBound :: a)
    maxInt = fromEnum (maxBound :: a)

preimage' :: (Eq b, Bounded a, Enum a) => (a -> b) -> b -> [a]
preimage' f b = preimage f b every

mToE :: e -> Maybe a -> Either e a
mToE e = maybe (Left e) Right

eToM :: Either e a -> Maybe a
eToM = either (const Nothing) Just

bToE :: Bool -> e -> Either e ()
bToE b e = if b
  then Right ()
  else Left e

($>>) :: a -> (a -> b) -> b
($>>) = flip ($)
infixl 0 $>>

(<<$) :: (a -> b) -> a -> b
(<<$) = ($)
infixl 0 <<$

bToM :: (a -> Bool) -> a -> Maybe a
bToM test a = if test a
  then Just a
  else Nothing

debug :: Show a => String -> a -> a
debug s a = trace (s <> show a) a

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs)   | x `Prelude.elem` xs   = rmdups xs
                | otherwise     = x : rmdups xs

(.*) :: Monoid a => Natural -> a -> a
n .* a = if n == 0
  then mempty
  else ((n - 1) .* a) <> a
