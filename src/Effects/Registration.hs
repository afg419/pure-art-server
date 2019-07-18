{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module Effects.Registration where

import PointGen.Bip32
import Model
import Import
import Effects.Common
import Effects.Interpreters

class (Effect s, MonadIO s) => Registration s where
  insertXPub :: XPub -> s SXPubId

instance Registration PsqlDB where
  insertXPub xpub = do
    now <- liftIO getCurrentTime
    PsqlDB <<< fmap SXPubId <<< insert $ XPubRecord xpub now now
