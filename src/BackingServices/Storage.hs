{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}

module BackingServices.Storage where

import PointGen.Bip32
import PointGen.Address
import PointGen.Plane
import PointGen.Coordinate
import PointGen.AddressTransform
import Model
import BackingServices.Common
import Import

newtype Eff s = Eff { ex :: forall a. s a -> Handler a}

class (Effect s, MonadIO s) => Registration s where
  insertXPub :: XPub -> s SXPubId

class Effect s => CanvasGeneration s where
  -- returns id of record, and 0/0 address
  insertCanvas2 :: SAsset a -> SXPubId -> Plane2 m n -> s (SCanvas2Id m n a, Address a)
  -- lookup hotwallet, if sufficient funds begin generating addresses
  startGeneration :: SCanvas2Id m n a -> s (Either String ())
  -- (missing coordinates, found coordinates)
  getPlane2Locales :: SCanvas2Id m n a -> s ([Coordinate m n] ,[Locale m n a])

newtype PsqlDB a = PsqlDB { runPsql :: SqlPersistT Handler a } deriving (Functor, Applicative, Monad, MonadIO)

instance Effect PsqlDB where
  run = runDB <<< runPsql

instance Registration PsqlDB where
  insertXPub xpub = do
    now <- liftIO getCurrentTime
    PsqlDB <<< fmap SXPubId <<< insert $ XPubRecord xpub now now
--
-- instance CanvasGeneration PsqlDB where


-- testF :: (Registration s, Storage s) => SAsset a -> XPub -> Plane2 m n -> s (Address a)
-- testF asset xpub plane = do
--   xpubId <- insertXPub asset xpub
--   (_, address) <- insertCanvas2 asset xpubId plane
--   pure address
--
-- testG :: (Registration s, Storage t) => SAsset a -> XPub -> Reader (Eff s, Eff t) (Handler SXPubId)
-- testG asset xpub = do
--   (runS, runT) <- ask
--   pure $ ex runS $ insertXPub asset xpub
--
-- testI :: SAsset a -> XPub -> Handler SXPubId
-- testI asset xpub = runIdentity $ flip runReaderT (Eff (run @DarkDB), Eff (run @PsqlDB)) $ testG asset xpub


-- runTestF :: SAsset a -> XPub -> Plane2 m n -> Handler (Address a)
-- runTestF = testF
