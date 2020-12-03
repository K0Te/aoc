{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified Data.Attoparsec.Text as PT
import Data.List (cycle)
import RIO.List.Partial ((!!))
import Data.List (head)

data Point = Tree | Empty deriving (Eq, Show)

parsePoint :: Char -> Point
parsePoint '.' = Empty
parsePoint '#' = Tree

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/3.txt"
    let input = (fmap.fmap) parsePoint (T.unpack <$> T.lines fileData) 
        repeatedInput = cycle <$> input
    logInfo (displayShow $ solve1 repeatedInput)
    logInfo (displayShow $ solve2 repeatedInput)

solve1 :: [[Point]] -> Int
solve1 input = length $ filter (==Tree) (followPath input)

followPath :: [[Point]] -> [Point]
followPath inp = let (off, res) = foldl' (\(offset, points) line -> (offset+3, (line !! offset) : points)) (0, []) inp in res

everyf :: Int -> [a] -> [a]
everyf n [] = []
everyf n as  = head as : everyf n (drop n as)

followPath2 :: (Int, Int) -> [[Point]] -> [Point]
followPath2 (dx, dy) inp = let (off, res) = foldl' (\(offset, points) line -> (offset+dx, (line !! offset) : points)) (0, []) (everyf dy inp) in res

solve2  :: [[Point]] -> Int
solve2 input = (length $ filter (==Tree) (followPath2 (1, 1) input)) *
    (length $ filter (==Tree) (followPath2 (3, 1) input)) * 
    (length $ filter (==Tree) (followPath2 (5, 1) input)) * 
    (length $ filter (==Tree) (followPath2 (7, 1) input)) * 
    (length $ filter (==Tree) (followPath2 (1, 2) input))
