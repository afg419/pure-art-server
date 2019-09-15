{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Paint where

import Import hiding (undefined, natVal)
import Effects.CanvasGeneration
import Effects.Interpreters
import Effects.Common
import PointGen
import Paint
import Model
import Control.Monad.Except

data PaintScaffoldReq = PaintScaffoldReq
  { xpub :: XPub
  , image :: Graph Coordinate2
  , asset :: Asset
  , xSize :: Natural
  , ySize :: Natural
  } deriving Generic
instance FromJSON PaintScaffoldReq

data SPaintScaffoldReq a m n = SPaintScaffoldReq
  (SCTY a m n)
  (Entity PublicKeyGenerator)
  (Graph (SCoordinate2 m n))

data PaintScaffoldRes v where
  PaintScaffoldRes :: [TxScaffold (BranchCounter v)] -> PaintScaffoldRes v
deriving instance Show v => (Show (PaintScaffoldRes v))
deriving instance (Functor PaintScaffoldRes)
instance ToJSON v => ToJSON (PaintScaffoldRes v) where
  toJSON (PaintScaffoldRes scaffoldList) = toJSON scaffoldList

data DpSLocale (a :: Asset) (m :: Nat) (n :: Nat) = DpSLocale
  { dPath :: DerivationPath
  , sLocale :: (SLocale a m n)
  } deriving (Show, Eq)

instance ToJSON (DpSLocale a m n) where
  toJSON (DpSLocale dp SLocale{..}) = object
    [ "coordinate" .= array [cx lCoordinate, cy lCoordinate]
    , "address" .= (String $ tshow lAddress)
    , "path" .= (String $ tshow dp)
    ]

postScaffoldPaintR :: Handler Value
postScaffoldPaintR = do
  canvasGenReq <- requireCheckJsonBody
  eRes <- runEffects (run @PsqlDB) <<< liftEffectful <<$ scaffoldBestFitPaintLogic canvasGenReq
  either (sendResponseStatus status500) pure eRes

mkSGraph :: SCTY a m n
  -> Graph Coordinate2
  -> Maybe (Graph (SCoordinate2 m n))
mkSGraph cty graph = traverse testVertextOOB graph
  where
    testVertextOOB v = mkCoordinate (fst v) (snd v) (dim cty)

scaffoldBestFitPaintLogic :: CanvasGeneration r => PaintScaffoldReq -> r (Either Text Value)
scaffoldBestFitPaintLogic (PaintScaffoldReq xpub image ca cx cy) = runExceptT $ do
    pkgen <- ExceptT <<< fmap (mToE "Xpub not found") <<$ getPublicKeyGenerator xpub

    withCanvasTy cty $ \scty -> do
      sImage <- ExceptT <<< pure <<< mToE "vertices oob" <<$ mkSGraph scty image
      res <- ExceptT <<< fmap Right <<$ sScaffoldBestFitPaintLogic (SPaintScaffoldReq scty pkgen sImage)
      pure <<< toJSON <<$ res

    where
      cty = CTY ca cx cy

sScaffoldBestFitPaintLogic :: (CanvasGeneration r) => SPaintScaffoldReq a m n -> r (PaintScaffoldRes (DpSLocale a m n))
sScaffoldBestFitPaintLogic (SPaintScaffoldReq scty pkgen sImage) = do
  locales <- fmap (fmap toDpsLocale) <<< getPublicKeys <<$ pkgenId

  let localesImage = fmap (getClosestLocale locales) sImage
  let hotL = DpSLocale hotPath (hotLocale scty xpub)
  pure <<< PaintScaffoldRes <<$ graphToTxScaffold hotL localesImage
  where
    pkgenId = entityKey pkgen
    xpub = publicKeyGeneratorXpub <<$ entityVal pkgen
    getClosestLocale ls p = minOn (\l -> l1Dist (lCoordinate <<$ sLocale l) p) ls
    toDpsLocale PublicKeyRecord{..} =
      DpSLocale 
      publicKeyRecordPath
      (mkLocale scty publicKeyRecordPublicKey)
