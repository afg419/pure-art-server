{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}

module Effects.CanvasGeneration where

import PointGen
import Model
import Effects.Common
import Effects.Interpreters
import Database.Persist
import Data.Singletons
import Data.Singletons.TypeLits
import Data.Maybe (fromJust)
import Import hiding (undefined, Proxy)
-- import Data.Nat
import GHC.TypeNats

class Effect s => CanvasGeneration s where
  -- returns id of record, and 0/0 address
  insertCanvas2 :: KnownNats m n => XPub -> SAsset a -> Plane2 m n -> s (Either String (SCanvas2Id a m n))
  getCanvas2    :: Canvas2Id -> s (Maybe Canvas2)
  sGetCanvas2    :: SCanvas2Id m n a -> s (Maybe (SCanvas2 m n a))
  updateCanvas2NextPathIndex :: SCanvas2Id a m n -> Integer -> s ()

  -- this uses a repsertMany so no duplicate coordinates stored
  insertPlane2Locales :: KnownNats m n => SAsset a -> SCanvas2Id a m n -> [SLocale a m n] -> s ()

  getPlane2Locales :: KnownNats m n => SAsset a -> Canvas2Id -> s [SLocale a m n]

-- withCanvasId :: CanvasGeneration r => Canvas2Id -> ( forall m n a. (KnownNat m, KnownNat n) => SAsset a -> SCanvas2 a m n -> s ) -> r (Maybe s)
-- withCanvasId cid f = do
--   mCanvas <- getCanvas2 cid
--   case mCanvas of
--     Just c -> pure <<< Just $ withCanvas c f
--     Nothing -> pure $ Nothing
--
-- withCanvas :: Canvas2 -> ( forall m n a. ( KnownNat m, KnownNat n) => SAsset a -> SCanvas2 a m n -> s ) ->  s
-- withCanvas c f = fromJust $ withSomeSing asset $ \sAsset ->
--     case (someNatVal xdim, someNatVal ydim) of
--       (Just (SomeNat (Proxy :: Proxy m)), Just (SomeNat (Proxy :: Proxy n))) ->
--         (SCanvas2 c :: SCanvas2 a m n) $>> f sAsset >>> Just
--       _ -> Nothing
--   where
--     xdim = fromIntegral <<< canvas2XSize <<$ c
--     ydim = fromIntegral <<< canvas2YSize <<$ c
--     asset = canvas2Asset <<$ c

instance CanvasGeneration PsqlDB where
  insertCanvas2 xpub sAsset p2 = do
    now <- liftIO getCurrentTime
    canvas2Id <- PsqlDB <<< insert <<$ canvas now
    pure <<< Right <<< SCanvas2Id <<$ canvas2Id
      where
        (xSize, ySize) = plane2Dim p2
        canvas = \now -> Canvas2
          xpub
          (fromIntegral xSize)
          (fromIntegral ySize)
          (fromSing sAsset)
          0
          now
          now
  getCanvas2 cId = PsqlDB <<< get $ cId
  sGetCanvas2 (SCanvas2Id cid) = fmap (fmap SCanvas2) <<$ getCanvas2 cid
  updateCanvas2NextPathIndex (SCanvas2Id cId) nextIndex = [Canvas2NextPathIndex =. (fromIntegral nextIndex)]
    $>> update cId
    >>> PsqlDB
  insertPlane2Locales sAsset scId locales = do
    now <- liftIO getCurrentTime
    PsqlDB <<< repsertMany $ fmap ( toLocaleRecordKey scId &&& (withSingI sAsset) (toLocaleRecord scId now)) locales
  getPlane2Locales a cid =
    selectList [ LocaleRecordCanvas2Id ==. cid] []
    $>> fmap ((fmap $ entityVal >>> fromLocaleRecord a))
    >>> PsqlDB


-- withPromoted :: l -> (Integer, Integer, Asset) -> ( forall a m n. (KnownNats m n, SingI a) => l -> s ) -> s
-- withPromoted l (i,j,a) f = withSomeSing a $ \(sAsset :: SAsset a) ->
--     case (someNatVal i, someNatVal j) of
--       (Just (SomeNat (Proxy :: Proxy m)), Just (SomeNat (Proxy :: Proxy n))) -> (withKnownNat (SNat :: Sing m) $ withKnownNat (SNat :: Sing n) $ (withSingI sAsset f l))
--       _ -> error "Dimensions negative"
--
-- -- withSingsI :: SNat m -> SNat n -> SAsset a  -> ((SingI m, SingI n, SingI a) => s ) -> s
-- withSingsI sm sn sAsset s = case (singInstance sm, singInstance sn, singInstance sAsset) of
--   (SingInstance :: SingInstance m, SingInstance :: SingInstance n, SingInstance :: SingInstance a) -> s

data DimTy m n where
  DimTy :: forall (m :: Nat) (n :: Nat). Proxy m -> Proxy n -> DimTy m n

newtype CTY a m n = CTY (SAsset a, Proxy m, Proxy n)

withDimTy :: (Natural, Natural) -> (forall a m n. (KnownNats m n) => DimTy m n -> r) -> r
withDimTy (i, j) f = case (someNatVal i, someNatVal j) of
  (SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) -> f $ DimTy (Proxy @m) (Proxy @n)

withCanvasTy :: (Asset, Natural, Natural) -> (forall (a :: Asset) m n. KnownNats m n => CTY a m n -> r) -> r
withCanvasTy (a, i, j) f = case (someAsset a, someNatVal i, someNatVal j) of
  (SomeSing (s :: SAsset a), SomeNat (Proxy :: Proxy m), SomeNat (Proxy :: Proxy n)) ->
    f $ CTY (s, Proxy @m, Proxy @n)
