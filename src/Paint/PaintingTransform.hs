module Paint.PaintingTransform where

data TxPainting = TxPainting { sourceTxs :: [Tx], paintingTxs :: [Tx] }

paintingToTransactions :: Painting2 a m n -> [Utxo] -> TxPainting
paintingToTransactions p =
  where
    vs = verticesG2 p
    vertexStars = vs $>> fmap (vertexEdgesG2 p) >>> sortOn starSize
