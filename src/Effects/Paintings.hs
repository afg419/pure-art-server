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

newtype SPainting (m :: Nat) (n :: Nat) = SPainting (Graph (SCoordinate m n))
class Effect s => Paintings s where
  insertPainting :: SContext a m n -> XPub -> SPainting m n -> s (ValidForContext a m n PaintingRecordId)
  retrievePaintingVertices :: SContext a m n -> ValidForContext a m n PaintingRecordId -> s [VertexRecordApproximation a m n]
  retrievePainting :: SContext a m n -> ValidForContext a m n PaintingRecordId -> s (Either Text (ValidForContext a m n PaintingRecord))
  updatePaintingIndex :: PaintingRecordId -> Natural -> s ()
  markPaintingFullyApproximated :: PaintingRecordId -> s ()
  retrieveParitallyApproximatedPaintings :: s [Entity PaintingRecord]
  replaceVertexLocale :: ValidForPlane m n VertexRecord -> SLocale a m n -> s ()

instance Paintings PsqlDB where
  insertPainting ctx@(SContext sa _) xpub (SPainting g@(Graph es)) = PsqlDB $ do
    now <- liftIO getCurrentTime

    precId <- insert <<$ PaintingRecord (getAsset ctx) (fromIntegral <<< getXDim $ ctx) (fromIntegral <<< getYDim $ ctx) xpub 0 False now now
    let paintingVertices = verticesG g

    let vertexRecords = fmap (vertexToVertexRecord precId now) paintingVertices
    insertMany_ vertexRecords

    let edgeRecords = fmap (edgeToEdgeRecord precId now) es
    insertMany_ edgeRecords
    pure <<< flip unsafeMkValid sa <<$ precId

  retrievePaintingVertices (SContext sa p2) sPrecId = PsqlDB $ do
    vs <- let
        vertexGets = selectList [ VertexRecordPaintingRecordId ==. unwrapValidContext sPrecId] []
        mapGetToSafe2D = entityVal >>> mkSafeCoordinate p2
      in vertexGets $>> fmap (mapMaybe mapGetToSafe2D)
      --  $>> fmap (fmap (entityVal >>> mkSafe2D WithinRange p2) $>> mapMaybe)
    for vs $ \v -> case vertexRecordClosestLocale $ unwrapValid v of
        Nothing -> pure <<$ NoRecord v
        Just localeId -> do
          locale <- fmap (flip unsafeMkValid sa <<< fromJust) <<$ get localeId -- TODO :: replace with primary keys
          if l1Dist v locale == 0
            then pure <<$ PerfectRecord v locale
            else pure <<$ ApproximateRecord v locale

  retrievePainting ctx sPrecId = PsqlDB $ do
    mPainting <- get (unwrapValidContext sPrecId)
    case mPainting of
      Nothing -> Left "Painting not found" $>> pure
      Just painting -> mkSafe ctx painting $>> mToE "Painting cty does not match" >>> pure

  updatePaintingIndex precId nextIndex = [PaintingRecordNextPathIndex =. fromIntegral nextIndex]
    $>> update precId
    >>> PsqlDB

  markPaintingFullyApproximated precId = [PaintingRecordFullyApproximated =. True]
    $>> update precId
    >>> PsqlDB

  replaceVertexLocale v sLocale = PsqlDB $ do
    let vertex = unwrapValid v
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

vertexToVertexRecord :: PaintingRecordId -> UTCTime -> SCoordinate m n -> VertexRecord
vertexToVertexRecord prid t s = VertexRecord prid (fromIntegral $ getX s) (fromIntegral $ getY s) Nothing t

edgeToEdgeRecord :: PaintingRecordId -> UTCTime -> Edge (SCoordinate m n) -> EdgeRecord
edgeToEdgeRecord prid t (Edge from' to') = EdgeRecord prid fromX fromY toX toY t
  where
    fromX = fromIntegral <<$ getX from'
    fromY = fromIntegral <<$ getY from'

    toX = fromIntegral <<$ getX to'
    toY = fromIntegral <<$ getY to'

edgeRecordToEdge :: EdgeRecord -> Edge Coordinate
edgeRecordToEdge (EdgeRecord _ fromX fromY toX toY _) =
  Edge
    (C (fromIntegral fromX) (fromIntegral fromY))
    (C (fromIntegral toX) (fromIntegral toY))
