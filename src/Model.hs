{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import PointGen.Address
import PointGen.Bip32
import Import

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

newtype SCanvas2 (m :: Nat) (n :: Nat) (a :: Asset) = SCanvas2 Canvas2
newtype SCanvas2Id (m :: Nat) (n :: Nat) (a :: Asset) = SCanvas2Id Canvas2Id
newtype SXPubId = SXPubId XPubRecordId deriving Show
