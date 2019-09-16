{-# LANGUAGE OverloadedStrings #-}

module Effects.Paintings where

import PointGen
import Model
import Paint
import Effects.Common
import Effects.Interpreters
import Database.Persist
import Import hiding (undefined)

data SPainting2 (m :: Nat) (n :: Nat) = SPainting2 (Graph (SCoordinate2 m n))
data Painting2 = Painting2 (Graph Coordinate2)

instance ToJSON Painting2 where
  toJSON (Painting2 g) = toJSON g

class Effect s => Paintings s where
  insertPainting :: PublicKeyGeneratorId -> Plane2 m n -> SPainting2 m n -> s PaintingRecordId
  retrievePainting :: PaintingRecordId -> s (Maybe Painting2)

instance Paintings PsqlDB where
  insertPainting pkgenId p2 (SPainting2 (Graph es)) = PsqlDB $ do
    now <- liftIO getCurrentTime

    let (xSize, ySize) = dimensions p2
    prid <- insert <<$ PaintingRecord False pkgenId (fromIntegral xSize) (fromIntegral ySize) now now
    let edgeRecords = fmap (edgeToEdgeRecord prid now) es
    insertMany_ edgeRecords
    pure prid

  retrievePainting prid = PsqlDB $ do
    mPainting <- get prid
    case mPainting of
      Nothing -> pure Nothing
      Just _ -> do
        edgeRecords <- fmap entityVal <$> selectList [ EdgeRecordPaintingRecordId ==. prid] []
        pure <<< Just <<< Painting2 <<< Graph <<< fmap edgeRecordToEdge <<$ edgeRecords

edgeToEdgeRecord :: PaintingRecordId -> UTCTime -> Edge (SCoordinate2 m n) -> EdgeRecord
edgeToEdgeRecord prid t (Edge from' to') = EdgeRecord prid fromX fromY toX toY t
  where
    fromX = fromIntegral <<$ cx from'
    fromY = fromIntegral <<$ cy from'
    toX = fromIntegral <<$ cx to'
    toY = fromIntegral <<$ cy to'

edgeRecordToEdge :: EdgeRecord -> Edge Coordinate2
edgeRecordToEdge (EdgeRecord _ fromX fromY toX toY _) =
  Edge
  (fromIntegral fromX, fromIntegral fromY)
  (fromIntegral toX, fromIntegral toY)
