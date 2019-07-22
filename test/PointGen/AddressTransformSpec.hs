module PointGen.AddressTransformSpec where

import Test.QuickCheck
import Gen.PointGen
import PointGen
import Import

prop_projectToTrip :: IO ()
prop_projectToTrip = quickCheck $ do

  join $ withRandomCoordinate $ \coord -> do
    let originalPlane = plane coord

    join $ withRandomPlane $ \p -> do
      let coord_down = projectTo p coord
      let fibre = fibreOver originalPlane coord_down
      coord_down_up <- genSubplaneCoordinate $ fibre
      let coord_down_up_down = projectTo p coord_down_up

      pure $
        coord_down == coord_down_up_down
        &&
        coord `inSubPlane` fibre
