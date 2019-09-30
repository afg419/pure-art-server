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
import Data.Maybe (fromJust)

data SPainting2 (m :: Nat) (n :: Nat) = SPainting2 (Graph (SCoordinate2 m n))
data Painting2 = Painting2 (Graph Coordinate2)

instance ToJSON Painting2 where
  toJSON (Painting2 g) = toJSON g

class Effect s => Paintings s where
  insertPainting :: SCTY a m n -> XPub -> SPainting2 m n -> s (SafeCTY a m n PaintingRecordId)
  retrievePaintingVertices :: Plane2 m n -> SafeCTY a m n PaintingRecordId -> s [VertexRecordApproximation a m n]
  retrievePainting :: SCTY a m n -> SafeCTY a m n PaintingRecordId -> s (Either Text (SafeCTY a m n PaintingRecord))
  updatePaintingIndex :: PaintingRecordId -> Natural -> s ()
  markPaintingFullyApproximated :: PaintingRecordId -> s ()
  retrieveParitallyApproximatedPaintings :: s [Entity PaintingRecord]
  replaceVertexLocale :: Safe2D m n VertexRecord -> SLocale a m n -> s ()

instance Paintings PsqlDB where
  insertPainting scty xpub (SPainting2 g@(Graph es)) = PsqlDB $ do
    now <- liftIO getCurrentTime

    precId <- insert <<$ PaintingRecord (getAsset scty) (fromIntegral <<< getX $ scty) (fromIntegral <<< getY $ scty) xpub 0 False now now
    let paintingVertices = verticesG g

    let vertexRecords = fmap (vertexToVertexRecord precId now) paintingVertices
    insertMany_ vertexRecords

    let edgeRecords = fmap (edgeToEdgeRecord precId now) es
    insertMany_ edgeRecords
    pure <<< toSafeCTY <<$ precId

  retrievePaintingVertices p2 sPrecId = PsqlDB $ do
    vs <- let
        vertexGets = selectList [ VertexRecordPaintingRecordId ==. fromSafeCTY sPrecId] []
        mapGetToSafe2D = entityVal >>> mkSafe2D WithinRange p2
      in vertexGets $>> fmap (mapMaybe mapGetToSafe2D)
       -- $>> fmap (fmap (entityVal >>> mkSafe2D WithinRange p2) $>> mapMaybe)
    for vs $ \v -> case onSafe2D vertexRecordClosestLocale v of
        Nothing -> pure <<$ NoRecord v
        Just localeId -> do
          locale <- fmap (toSafeCTY <<< fromJust) <<$ get localeId -- TODO :: replace with primary keys
          if l1Dist v locale == 0
            then pure <<$ PerfectRecord v locale
            else pure <<$ ApproximateRecord v locale

  retrievePainting scty sPrecId = PsqlDB $ do
    mPainting <- get (fromSafeCTY sPrecId)
    case mPainting of
      Nothing -> Left "Painting not found" $>> pure
      Just p -> mkSafePainting scty p $>> mToE "Painting cty does not match" >>> pure

  updatePaintingIndex precId nextIndex = [PaintingRecordNextPathIndex =. (fromIntegral nextIndex)]
    $>> update precId
    >>> PsqlDB

  markPaintingFullyApproximated precId = [PaintingRecordFullyApproximated =. True]
    $>> update precId
    >>> PsqlDB

  replaceVertexLocale v sLocale = PsqlDB $ do
    let vertex = fromSafe2D v
    now <- liftIO getCurrentTime
    let prid = vertexRecordPaintingRecordId vertex
    let localeRecord = localeToLocaleRecord prid now sLocale
    newLocaleId <- entityKey <$> upsert localeRecord []
    update (getVertexRecordId vertex) [VertexRecordClosestLocale =. Just newLocaleId]

  retrieveParitallyApproximatedPaintings = selectList
    [ PaintingRecordFullyApproximated ==. False ] []
    $>> PsqlDB

localeToLocaleRecord :: PaintingRecordId -> UTCTime -> SLocale a m n -> LocaleRecord
localeToLocaleRecord prid t sl@SLocale{..} = LocaleRecord prid lPath (tshow lAddress) lrx lry t
  where
    lrx = fromIntegral <<< getX <<$ sl
    lry = fromIntegral <<< getY <<$ sl

vertexToVertexRecord :: PaintingRecordId -> UTCTime -> SCoordinate2 m n -> VertexRecord
vertexToVertexRecord prid t (SCoordinate2{..}) = VertexRecord prid (fromIntegral cx) (fromIntegral cy) Nothing t

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
