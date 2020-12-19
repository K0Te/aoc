{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day17 where

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
import qualified RIO.Map as M
import RIO.List.Partial (minimum)
import RIO.List.Partial (maximum)
import RIO.List (cycle)
import Prelude (repeat)

type Pos = Maybe ()
type Coord = (Int, Int, Int, Int)
type Board = M.Map Coord Pos

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/17.txt"
    let input = TT.unpack <$> TT.lines fileData
        parsed = zip [0..] . fmap (zip [0..])  . (fmap.fmap) parse $ input
        board = M.fromList . filter (isJust . snd) $  concat $ fmap (\(x, v) -> fmap (\(y, v1) -> ((x,y,0, 0), v1)) v) parsed
    logInfo (displayShow $ input)
    logInfo (displayShow $ parsed)
    logInfo (displayShow $ board)
    logInfo (displayShow $ tick board)
    logInfo (displayShow $ tick . tick $  board)
    logInfo (displayShow $ tick . tick . tick $  board)
    logInfo (displayShow $ tick . tick . tick . tick . tick . tick $  board)
    logInfo (displayShow $ M.size . tick . tick . tick . tick . tick . tick $  board)

parse :: Char -> Pos
parse '.' = Nothing
parse '#' = Just ()

nearBy :: Board -> Coord -> [Coord]
nearBy b (xx, yy, zz, ww) =
    let nCoords = [(x,y,z,w) | x<-[xx-1..xx+1], y <- [yy-1..yy+1], z<-[zz-1..zz+1], w<-[ww-1..ww+1], (xx,yy,zz, ww) /= (x,y,z,w)]
    in filter (\k -> M.member  k b) nCoords

tick :: Board -> Board
tick b =
    let keys = M.keys b
        minX = minimum $ fmap (\(x,_,_,_) -> x) keys
        maxX = maximum $ fmap (\(x,_,_,_) -> x) keys
        minY = minimum $ fmap (\(_,y,_,_) -> y) keys
        maxY = maximum $ fmap (\(_,y,_,_) -> y) keys
        minZ = minimum $ fmap (\(_,_,z,_) -> z) keys
        maxZ = maximum $ fmap (\(_,_,z,_) -> z) keys
        minW = minimum $ fmap (\(_,_,_,w) -> w) keys
        maxW = maximum $ fmap (\(_,_,_,w) -> w) keys
        possiblePositions = [(x,y,z,w) | x<-[minX-1..maxX+1], y<-[minY-1..maxY+1], z <- [minZ-1..maxZ+1], w<-[minW-1..maxW+1]]
        newPositions = filter (isActive b) possiblePositions
    in M.fromList  $ zip newPositions (repeat $ Just ())

isActive :: Board -> Coord -> Bool
isActive b c =
    let nearCount = length $ nearBy b c
    in case M.lookup c b of
        Nothing -> if nearCount == 3 then True else False 
        Just _ -> if nearCount == 3 || nearCount == 2 then True else False 

-- singleTick :: Pos -> Int -> Pos
-- singleTick v count = case v of
--     Floor -> Floor
--     Free -> if count == 0 then Occupied else Free
--     Occupied -> if count >= 5 then Free else Occupied

-- emulate :: Board -> [Board]
-- emulate = go
--     where go old = let new = tick old in old:go new

-- solve1 :: Board -> Int
-- solve1 b = let final = go b in length $ filter (==Occupied) (concat . toList $ toList <$> state final)
--     where go old = let new = tick old in if old==new then old else go new
