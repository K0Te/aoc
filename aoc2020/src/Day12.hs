{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day12 where

import Import
import RIO.Partial (fromJust, read)
import qualified RIO.Text as T
import qualified RIO.Set as S
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial ((!!), head)
import Control.Monad.ST
import Data.Vector ((!?))
import RIO.Vector (imap)
import Data.Vector (fromList)

data Cmd = Forward Int | RightC Int | LeftC Int | South Int | North Int | East Int | West Int deriving (Eq, Show)
data Pos = Pos {x::Int, y::Int, dir::Int} deriving (Eq, Show)
data Pos2 = Pos2 {ax::Int, ay::Int, wp::(Int, Int)} deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/12.txt"
    let input = TT.unpack <$> TT.lines fileData
        parsed = parse <$> input
    logInfo (displayShow $ parsed)
    logInfo (displayShow $ solve1 parsed)
    logInfo (displayShow $ solve2 parsed)

norm :: Int -> Int
norm x
    | x >= 360 = x `mod` 360
    | x < 0 = norm $ x + 360
    | True = x

dirToXy :: Int -> (Int, Int)
dirToXy 0 = (-1, 0)
dirToXy 90 = (0, 1)
dirToXy 180 = (1, 0)
dirToXy 270 = (0, -1)
dirToXy v = trace (tshow v) (0,0)

parse :: String -> Cmd
parse ('F':xs) = Forward (read xs)
parse ('R':xs) = RightC (read xs)
parse ('L':xs) = LeftC (read xs)
parse ('N':xs) = North (read xs)
parse ('E':xs) = East (read xs)
parse ('W':xs) = West (read xs)
parse ('S':xs) = South (read xs)

step :: Pos -> Cmd -> Pos
step Pos{..} (North dy) = Pos{y=y+dy, ..}
step Pos{..} (South dy) = Pos{y=y-dy, ..}
step Pos{..} (East dx) = Pos{x=x+dx, ..}
step Pos{..} (West dx) = Pos{x=x-dx, ..}
step Pos{..} (RightC r) = Pos{dir=norm $ dir-r, ..}
step Pos{..} (LeftC l) = Pos{dir=norm $ dir+l, ..}
step Pos{..} (Forward v) =  let (dx, dy) = dirToXy dir in Pos{x=x+v*dx, y=y+v*dy, ..}

solve1 :: [Cmd] -> Pos
solve1 = foldl' step Pos{x=0, y=0, dir=180} 

wpRotate :: (Int, Int) -> Int -> (Int, Int)
wpRotate (x, y) 0 = (x,y)
wpRotate (x, y) 90
    | x >= 0 && y >= 0 = (-y, x)
    | x <= 0 && y >= 0 = (-y, x)
    | x <= 0 && y <= 0 = (-y, x)
    | x >= 0 && y <= 0 = (-y, x)
wpRotate i d = wpRotate (wpRotate i 90) (d-90)

step2 :: Pos2 -> Cmd -> Pos2
step2 Pos2{..} (North v) = Pos2{wp=(fst wp, snd wp + v), ..}
step2 Pos2{..} (South v) = Pos2{wp=(fst wp, snd wp - v), ..}
step2 Pos2{..} (East v) = Pos2{wp=(fst wp + v, snd wp), ..}
step2 Pos2{..} (West v) = Pos2{wp=(fst wp - v, snd wp), ..}
step2 Pos2{..} (RightC v) = Pos2{wp=wpRotate wp $ norm (-v), ..}
step2 Pos2{..} (LeftC v) = Pos2{wp=wpRotate wp $ norm v, ..}
step2 Pos2{..} (Forward v) = Pos2{ax=ax+v*fst wp, ay=ay+v*snd wp, ..}

solve2 :: [Cmd] -> Pos2
solve2 = foldl' step2 Pos2{ax=0, ay=0, wp=(10,1)} 