{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
  insertPainting :: SCTY a m n -> XPub -> SPainting2 m n -> s (Safe PaintingRecordId a m n)
  retrievePaintingVertices :: Plane2 m n -> Safe PaintingRecordId a m n  -> s [Safe VertexRecord a m n]
  retrievePainting :: SCTY a m n -> Safe PaintingRecordId a m n  -> s (Either Text (Safe PaintingRecord a m n))
  updatePaintingIndex :: PaintingRecordId -> Natural -> s ()

instance Paintings PsqlDB where
  insertPainting scty xpub (SPainting2 g@(Graph es)) = PsqlDB $ do
    now <- liftIO getCurrentTime

    let (CTY sa sx sy) = demoteSCTY scty
    precId <- insert <<$ PaintingRecord sa (fromIntegral sx) (fromIntegral sy) xpub 0 now now
    let paintingVertices = verticesG g

    let vertexRecords = fmap (vertexToVertexRecord precId now) paintingVertices
    insertMany_ vertexRecords

    let edgeRecords = fmap (edgeToEdgeRecord precId now) es
    insertMany_ edgeRecords
    pure <<$ Safe precId
  retrievePaintingVertices p2 sPrecId =
    selectList [ VertexRecordPaintingRecordId ==. fromSafe sPrecId] []
    $>> fmap (entityVal >>> mkSafeVertex p2 $>> mapMaybe) >>> PsqlDB
  retrievePainting scty sPrecId = PsqlDB $ do
    mPainting <- get (fromSafe sPrecId)
    case mPainting of
      Nothing -> Left "Painting not found" $>> pure
      Just p -> mkSafePainting scty p $>> mToE "Painting cty does not match" >>> pure
  updatePaintingIndex precId nextIndex = [PaintingRecordNextPathIndex =. (fromIntegral nextIndex)]
    $>> update precId
    >>> PsqlDB



vertexToVertexRecord :: PaintingRecordId -> UTCTime -> SCoordinate2 m n -> VertexRecord
vertexToVertexRecord prid t (SCoordinate2{..}) = VertexRecord prid (fromIntegral cx) (fromIntegral cy) Nothing t

edgeToEdgeRecord :: PaintingRecordId -> UTCTime -> Edge (SCoordinate2 m n) -> EdgeRecord
edgeToEdgeRecord prid t (Edge from' to') = EdgeRecord from to t
  where
    from = VertexRecordKey prid (fromIntegral <<$ cx from') (fromIntegral <<$ cy from')
    to = VertexRecordKey prid (fromIntegral <<$ cx to') (fromIntegral <<$ cy to')

edgeRecordToEdge :: EdgeRecord -> Edge Coordinate2
edgeRecordToEdge (EdgeRecord (VertexRecordKey _ fromX fromY) (VertexRecordKey _ toX toY) _) =
  Edge
  (fromIntegral fromX, fromIntegral fromY)
  (fromIntegral toX, fromIntegral toY)
