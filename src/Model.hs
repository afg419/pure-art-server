{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE RecordWildCards            #-}

module Model where

import ClassyPrelude.Yesod
import Database.Persist.Quasi
import PointGen
import GHC.TypeNats
import Import

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

-- data SPaintingRecord (a :: Asset) (m :: Nat) (n :: Nat) = SPaintingRecord PaintingRecord
-- data SPaintingRecordId (a :: Asset) (m :: Nat) (n :: Nat) = SPaintingRecordId PaintingRecordId
-- data SPaintingRecord (a :: Asset) (m :: Nat) (n :: Nat) = SPaintingRecord PaintingRecord

data Safe (s :: *) (a :: Asset) (m :: Nat) (n :: Nat) = Safe s
fromSafe :: Safe s a m n -> s
fromSafe (Safe s) = s

mkSafeVertex :: forall a m n. Plane2 m n -> VertexRecord -> Maybe (Safe VertexRecord a m n)
mkSafeVertex p2 v@VertexRecord{..} = case mkCoordinate vx vy p2 of
  Nothing -> Nothing
  Just _ -> Just <<$ Safe v
  where
    vx = fromIntegral vertexRecordX
    vy = fromIntegral vertexRecordY

mkSafePainting :: forall a m n. SCTY a m n -> PaintingRecord -> Maybe (Safe PaintingRecord a m n)
mkSafePainting scty p@PaintingRecord{..} = if demoteSCTY scty == CTY pa px py
  then Just <<$ Safe p
  else Nothing
  where
    px = fromIntegral paintingRecordXSize
    py = fromIntegral paintingRecordYSize
    pa = paintingRecordAsset
