{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.List
import Data.Maybe

-- <x=14, y=2, z=8>
-- <x=7, y=4, z=10>
-- <x=1, y=17, z=16>
-- <x=-4, y=-1, z=1>

data MoonCoord = MoonCoord { x :: Int, y :: Int, z :: Int} deriving (Show, Eq)
data MoonSpeed = MoonSpeed { dx :: Int, dy :: Int, dz :: Int} deriving (Show, Eq)
data MoonState = MoonState MoonCoord MoonSpeed deriving (Show, Eq)

zeroSpeed :: MoonSpeed
zeroSpeed = MoonSpeed 0 0 0

initialStates :: [MoonState]
initialStates = [
  MoonState (MoonCoord 14 2 8) zeroSpeed,
  MoonState (MoonCoord 7 4 10) zeroSpeed,
  MoonState (MoonCoord 1 17 16) zeroSpeed,
  MoonState (MoonCoord (-4) (-1) 1) zeroSpeed]
-- initialStates = [
--   MoonState (MoonCoord (-1) 0 2) zeroSpeed,
--   MoonState (MoonCoord 2 (-10) (-7)) zeroSpeed,
--   MoonState (MoonCoord 4 (-8) 8) zeroSpeed,
--   MoonState (MoonCoord 3 5 (-1)) zeroSpeed]

calcSingle :: Int -> Int -> Int
calcSingle coord_self coord_other
 | coord_self > coord_other = -1
 | coord_self < coord_other = 1
 | otherwise = 0

calcSpeed :: MoonState -> MoonState -> MoonState
calcSpeed (MoonState coord@MoonCoord{..} MoonSpeed{..}) (MoonState (MoonCoord other_x other_y other_z) _) =
  MoonState coord MoonSpeed{  dx = dx + calcSingle x other_x
                            , dy = dy + calcSingle y other_y
                            , dz = dz + calcSingle z other_z
                          }

applySpeed :: MoonState -> MoonState
applySpeed (MoonState MoonCoord{..} speed@MoonSpeed{..}) =
  MoonState (MoonCoord { x=x+dx, y=y+dy, z=z+dz}) speed

stepSpeed :: [MoonState] -> [MoonState]
stepSpeed moons = do
  moon <- moons
  return $ foldr (\other self -> calcSpeed self other) moon moons

stepCoord :: [MoonState] -> [MoonState]
stepCoord moons = applySpeed <$> moons

step :: [MoonState] -> [MoonState]
step = stepCoord . stepSpeed

calcEnergy :: MoonState -> Int
calcEnergy (MoonState MoonCoord{..} MoonSpeed{..}) =
  (abs x + abs y + abs z) * (abs dx + abs dy + abs dz)

-- Part 2
newtype CoordSingle = CoordSingle Int deriving (Show, Eq, Num)
newtype SpeedSingle = SpeedSingle Int deriving (Show, Eq, Num)
data StateSingle = StateSingle CoordSingle SpeedSingle deriving (Show, Eq)
-- instance Eq StateSingle where
--   (StateSingle coord1 _) == (StateSingle coord2 _) = coord1 == coord2

calcSpeedSingle :: StateSingle -> StateSingle -> StateSingle
calcSpeedSingle (StateSingle (CoordSingle self_coord) (SpeedSingle self_speed)) (StateSingle (CoordSingle other_coord) _) =
  StateSingle (CoordSingle self_coord) (SpeedSingle $ self_speed + calcSingle self_coord other_coord)

stepSpeedSingle :: [StateSingle] -> [StateSingle]
stepSpeedSingle moons = do
  moon <- moons
  return $ foldr (\other self -> calcSpeedSingle self other) moon moons

stepCoordSingle :: [StateSingle] -> [StateSingle]
stepCoordSingle moons = (\(StateSingle (CoordSingle coord) (SpeedSingle speed)) ->
                    StateSingle (CoordSingle $ coord+speed) (SpeedSingle speed)) <$> moons

stepSingle :: [StateSingle] -> [StateSingle]
stepSingle = stepCoordSingle . stepSpeedSingle

singleX = [
  StateSingle (CoordSingle 14) (SpeedSingle 0),
  StateSingle (CoordSingle 7) (SpeedSingle 0),
  StateSingle (CoordSingle 1) (SpeedSingle 0),
  StateSingle (CoordSingle (-4)) (SpeedSingle 0)
  ]
singleY = [
  StateSingle (CoordSingle 2) (SpeedSingle 0),
  StateSingle (CoordSingle 4) (SpeedSingle 0),
  StateSingle (CoordSingle 17) (SpeedSingle 0),
  StateSingle (CoordSingle (-1)) (SpeedSingle 0)
  ]
singleZ = [
  StateSingle (CoordSingle 8) (SpeedSingle 0),
  StateSingle (CoordSingle 10) (SpeedSingle 0),
  StateSingle (CoordSingle 16) (SpeedSingle 0),
  StateSingle (CoordSingle 1) (SpeedSingle 0)
  ]
-- singleX = [
--   StateSingle (CoordSingle (-8)) (SpeedSingle 0),
--   StateSingle (CoordSingle 5) (SpeedSingle 0),
--   StateSingle (CoordSingle 2) (SpeedSingle 0),
--   StateSingle (CoordSingle 9) (SpeedSingle 0)
--   ]
-- singleY = [
--   StateSingle (CoordSingle (-10)) (SpeedSingle 0),
--   StateSingle (CoordSingle 5) (SpeedSingle 0),
--   StateSingle (CoordSingle (-7)) (SpeedSingle 0),
--   StateSingle (CoordSingle (-8)) (SpeedSingle 0)
--   ]
-- singleZ = [
--   StateSingle (CoordSingle 0) (SpeedSingle 0),
--   StateSingle (CoordSingle 10) (SpeedSingle 0),
--   StateSingle (CoordSingle 3) (SpeedSingle 0),
--   StateSingle (CoordSingle (-3)) (SpeedSingle 0)
--   ]

main :: IO ()
main = do
  let states = (iterate step initialStates) !! 1000
  print $ sum (calcEnergy <$> states)
  let singleXSteps = iterate stepSingle singleX
  let singleYSteps = iterate stepSingle singleY
  let singleZSteps = iterate stepSingle singleZ
  -- print $ elemIndex (initialStates) (tail (iterate step initialStates)) -- not optimal :(
  let stepsXCount = 1 + (fromJust $ elemIndex (singleX) (tail singleXSteps))
  let stepsYCount = 1 + (fromJust $ elemIndex (singleY) (tail singleYSteps))
  let stepsZCount = 1 + (fromJust $ elemIndex (singleZ) (tail singleZSteps))
  print "X_steps="
  print stepsXCount
  print "Y_steps="
  print stepsYCount
  print "Z_steps="
  print stepsZCount
  print $ lcm (lcm stepsXCount stepsYCount) stepsZCount
