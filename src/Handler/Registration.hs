{-# LANGUAGE LiberalTypeSynonyms #-}

module Handler.Registration where

import Import
import Effects.Registration
import Model
import PointGen
import Effects.Common
import Effects.Interpreters

postRegisterXPubR :: Handler Text
postRegisterXPubR = do
  xpub <- requireCheckJsonBody
  fmap tshow <<< runEffects (run @PsqlDB) $ registerXPubLogic xpub

registerXPubLogic :: Registration r => XPub -> Effectful (Interpreter r) SXPubId
registerXPubLogic xpub = do
  i <- interpret <$> ask
  pure <<< i $ insertXPub xpub
