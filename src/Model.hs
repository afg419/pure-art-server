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
import Import

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

mkSafeVertex :: forall a m n. Plane2 m n -> VertexRecord -> Maybe (Safe VertexRecord a m n)
mkSafeVertex p2 v@VertexRecord{..} = case mkCoordinate (getX v) (getY v) p2 of
  Nothing -> Nothing
  Just _ -> Just <<$ Safe v

mkSafePainting :: forall a m n. SCTY a m n -> PaintingRecord -> Maybe (Safe PaintingRecord a m n)
mkSafePainting scty p@PaintingRecord{..} = if scty `ctyEq` p
  then Just <<$ Safe p
  else Nothing

vertexRecordId :: Safe VertexRecord a m n -> VertexRecordId
vertexRecordId (Safe VertexRecord{..}) =
  VertexRecordKey
    vertexRecordPaintingRecordId
    vertexRecordX
    vertexRecordY

instance TwoDimensional LocaleRecord where
  getX = fromIntegral <<< localeRecordX
  getY = fromIntegral <<< localeRecordY

instance TwoDimensional LocaleRecordId where
  getX (LocaleRecordKey _ x _) = fromIntegral <<$ x
  getY (LocaleRecordKey _ _ y) = fromIntegral <<$ y

instance TwoDimensional VertexRecord where
  getX = fromIntegral <<< vertexRecordX
  getY = fromIntegral <<< vertexRecordY

instance TwoDimensional (Safe VertexRecord a m n) where
  getX = getX <<< fromSafe
  getY = getY <<< fromSafe

instance TwoDimensional VertexRecordId where
  getX (VertexRecordKey _ x _) = fromIntegral x
  getY (VertexRecordKey _ _ y) = fromIntegral y

instance CTY PaintingRecord where
  ctyAsset = paintingRecordAsset
  ctyX = fromIntegral <<< paintingRecordXSize
  ctyY = fromIntegral <<< paintingRecordYSize

instance CTY (Entity PaintingRecord) where
  ctyAsset = ctyAsset <<< entityVal
  ctyX = ctyX <<< entityVal
  ctyY = ctyY <<< entityVal

sEntityVal :: Safe (Entity record) a m n -> Safe record a m n
sEntityVal = Safe <<< entityVal <<< fromSafe

sEntityKey :: Safe (Entity record) a m n -> Safe (Key record) a m n
sEntityKey = Safe <<< entityKey <<< fromSafe

data VertexApproximation = Perfect | Approximate | NonExistent deriving Eq
classifyVertexRecord' :: VertexRecord -> VertexApproximation
classifyVertexRecord' v = case vertexRecordClosestLocale v of
  Nothing -> NonExistent
  Just lrk -> if l1Dist lrk v == 0
      then Perfect
      else Approximate
classifyVertexRecord :: Safe VertexRecord a m n -> VertexApproximation
classifyVertexRecord = classifyVertexRecord' <<< fromSafe
