module Paint.Transaction where

import qualified Network.Haskoin.Network as H

newtype Transaction = Transaction { runTx :: H.Tx }
