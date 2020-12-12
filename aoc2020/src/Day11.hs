{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day11 where

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

data Pos = Floor | Free | Occupied deriving (Eq, Show)
data Board = Board {rows :: Int, columns :: Int, state :: Vector (Vector Pos)} deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/11.txt"
    let input = TT.unpack <$> TT.lines fileData
        colCount = length (head input)
        rowCount = length input
        parsed = (fmap.fmap) parse input
        board = Board{columns=colCount, rows=rowCount, state=fromList $ fromList <$> parsed}
    logInfo (displayShow $ input)
    logInfo (displayShow $ solve1 board)
    -- logInfo (displayShow $ solve2 input)

parse :: Char -> Pos
parse '.' = Floor
parse 'L' = Free
parse '#' = Occupied

lookupV1 :: Vector (Vector a) -> (Int, Int) -> Maybe a
lookupV1 state (y, x) = 
    let mY = state !? y
    in join $ (!? x) <$> mY

nearByV1 :: Board -> (Int, Int) -> [Pos]
nearByV1 Board{..} (x, y) =
    let mLookup = lookupV1 state
        mNear = [
         mLookup (y-1, x-1),
         mLookup (y-1, x),
         mLookup (y-1, x+1),
         mLookup (y, x-1),
         mLookup (y, x+1),
         mLookup (y+1, x-1),
         mLookup (y+1, x),
         mLookup (y+1, x+1)
         ]
    in catMaybes mNear

nearByV2 :: Board -> (Int, Int) -> [Pos]
nearByV2 Board{..} (x, y) =
    let lookupDirection (y, x) dy dx = case
            mLookup (y+dy, x+dx) of
                Nothing -> Nothing
                Just Floor -> lookupDirection (y+dy, x+dx) dy dx
                Just result -> Just result
        mLookup = lookupV1 state
        lDir = lookupDirection (y, x)
        mNear = [
         lDir (-1) (-1),
         lDir (-1) 0,
         lDir (-1) 1,
         lDir 0 (-1),
         lDir 0 1,
         lDir 1 (-1),
         lDir 1 0,
         lDir 1 1
         ]
    in catMaybes mNear

tick :: Board -> Board
tick b@Board{..} =
    let newState = imap posTickY state
        posTickY :: Int -> Vector Pos -> Vector Pos
        posTickY y v = imap (posTickX y) v
        posTickX y x v = let count = length (filter (==Occupied) (nearByV2 b (x, y))) in singleTick v count
    in Board{state=newState, ..}
                
singleTick :: Pos -> Int -> Pos
singleTick v count = case v of
    Floor -> Floor
    Free -> if count == 0 then Occupied else Free
    Occupied -> if count >= 5 then Free else Occupied

emulate :: Board -> [Board]
emulate = go
    where go old = let new = tick old in old:go new

solve1 :: Board -> Int
solve1 b = let final = go b in length $ filter (==Occupied) (concat . toList $ toList <$> state final)
    where go old = let new = tick old in if old==new then old else go new
