module Paint.PaintingTransform where

import PointGen
import Import hiding ((\\))
import Paint.Graph
import Paint.Transaction
import Paint.Painting
import Data.List ((\\))

                                             
-- paintingTransform :: Painting2 a m n -> TxPainting a m n
-- paintingTransform p = _
--   where
--     components = connectedComponents [] p
--
-- -- single component transform
-- paintingTransform' :: Painting2 a m n -> SomeScaffold
-- paintingTransform' (Graph []) = TxPainting [] (InitTxScaffold []) []
-- paintingTransform' (Graph first:es) =
--   where
--     starts = [eSrc first]
