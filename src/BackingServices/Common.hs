module BackingServices.Common where

import Import

class Monad s => Effect s where
  run :: s a -> Handler a
