module PointGen.AddressTransformSpec where

import Test.QuickCheck
import Test.QuickCheck.Assertions ((<=?))
import Gen.PointGen
import PointGen
import Import hiding (print)
import Data.Maybe

prop_projectTo :: IO ()
prop_projectTo = quickCheck $ do
  withRandomPlane (const True) $ \plane_up -> do
    withRandomPlane (`stockLeqThan` plane_up) $ \plane_down -> do
      let (x_up_dim, y_up_dim) = dimensions plane_up
      let (x_down_dim, y_down_dim) = dimensions plane_down

      let sw_up = fromJust $ mkCoordinate 0 0 plane_up
      let nw_up = fromJust $ mkCoordinate 0 (y_up_dim - 1) plane_up
      let se_up = fromJust $ mkCoordinate (x_up_dim - 1) 0 plane_up
      let ne_up = fromJust $ mkCoordinate (x_up_dim - 1) (y_up_dim - 1) plane_up

      let sw_down = fromJust $ mkCoordinate 0 0 plane_down
      let nw_down = fromJust $ mkCoordinate 0 (y_down_dim - 1) plane_down
      let se_down = fromJust $ mkCoordinate (x_down_dim - 1) 0 plane_down
      let ne_down = fromJust $ mkCoordinate (x_down_dim - 1) (y_down_dim - 1) plane_down

      pure $ ne_down == projectTo plane_down ne_up
        && nw_down == projectTo plane_down nw_up
        && sw_down == projectTo plane_down sw_up
        && se_down == projectTo plane_down se_up

prop_fibreOver :: IO ()
prop_fibreOver = quickCheck $ do
  let p_up = P2 @100 @100
  let p_down = P2 @10 @10
  coord_down <- genCoordinate p_down
  let fibre = fibreOver p_up coord_down

  pure $ (diameter <<< xRange $ fibre) === 10 -- && (diameter <<< yRange $ fibre) == 10

prop_projectToTrip :: IO ()
prop_projectToTrip = quickCheck $ do
  let plane_up = xLargePlane
  coord_up <- genCoordinate plane_up

  withRandomPlane (`stockLeqThan` plane_up) $ \plane_down -> do
    let coord_down = projectTo plane_down coord_up
    let fibre = fibreOver plane_up coord_down
    coord_down_up <- genSubplaneCoordinate fibre
    let coord_down_up_down = projectTo plane_down coord_down_up

    pure $
      coord_down === coord_down_up_down
      -- &&
      -- coord_up   `inSubPlane` fibre

prop_primageEquallyDistributed :: IO ()
prop_primageEquallyDistributed = quickCheck $ do
  let p_up = fibrePlane
  withRandomPlane (`stockLeqThan` p_up) $ \p_down -> do
    g1 <- genFibreCoordinate
    g2 <- genFibreCoordinate

    let coord_down_1 = projectTo p_down g1
    let coord_down_2 = projectTo p_down g2

    let fibre1 = p_up `fibreOver` coord_down_1
    let fibre2 = p_up `fibreOver` coord_down_2

    let xWidth1 = xRange fibre1
    let xWidth2 = xRange fibre2

    let yWidth1 = yRange fibre1
    let yWidth2 = yRange fibre2

    xOrY <- arbitrary
    pure $ if xOrY
      then (diameter xWidth1 - diameter xWidth2) <=? 1
      else (diameter yWidth1 - diameter yWidth2) <=? 1
