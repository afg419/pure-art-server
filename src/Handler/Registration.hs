module Handler.Registration where

import Import
import BackingServices.Storage
import BackingServices.Common

postRegisterXPubR :: Handler Text
postRegisterXPubR = do
  xpub <- requireCheckJsonBody
  fmap tshow <<< run @PsqlDB $ insertXPub xpub
