module Handler.Registration where

import Import
import BackingServices.Storage
import Model
import PointGen.Bip32
import Effects

postRegisterXPubR :: Handler Text
postRegisterXPubR = do
  xpub <- requireCheckJsonBody
  fmap tshow <<< runEffects (Eff $ run @PsqlDB) $ registerXPubLogic xpub

registerXPubLogic :: Registration r => XPub -> Effectful (Eff r) SXPubId
registerXPubLogic xpub = do
  runR <- ask
  pure <<< ex runR $ insertXPub xpub
