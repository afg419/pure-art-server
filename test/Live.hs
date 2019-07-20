{-# LANGUAGE OverloadedStrings #-}

module Live where

import PointGen.Address
import PointGen.AddressTransform
import PointGen.Bip32
import PointGen.Plane
import PointGen.Coordinate
import PointGen.Range
import Import
import Data.Maybe (fromJust)

xpubT :: XPub
xpubT = fromJust <<< parseXPub $ "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8"

planeSmall :: Plane2 50 75
planeSmall = P2
planeMedium :: Plane2 250 300
planeMedium = P2
planeLarge :: Plane2 1000 1200
planeLarge = P2
planeXLarge :: Plane2 5000 6000
planeXLarge = P2

coordinateT :: KnownNats m n => Plane2 m n -> Coordinate2 m n
coordinateT p2 = Coordinate2 { x = 0, y = 0, plane = p2 }

testT :: KnownNats m n => Plane2 m n -> Maybe (Locale m n 'DOGE)
testT p2 = coordinateHunt SDOGE xpubT (coordinateT p2)
