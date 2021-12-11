{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec      as AT
import qualified Data.Attoparsec.Text as ATT
import           File                 (readLinesT)
import           Import               hiding (Down, to, (%~))
import           Prelude              (print)
import qualified RIO.Text             as T

--- ~1k rows and columns
--- ~1m points
--- ~500 rectangles
--- 500m lookup operations

main :: IO ()
main = do
    lines_ <- readLinesT "data/5.txt"
    print lines_
    print $ parsePuzzle (T.unlines lines_)
    print $ solve1 <$> parsePuzzle (T.unlines lines_)
    print $ solve2 <$> parsePuzzle (T.unlines lines_)

type Rec = (Int, Int, Int, Int)
type Point = (Int, Int)
newtype Puzzle = Puzzle [(Int, Int, Int, Int)]
    deriving (Eq, Show)

parsePuzzle :: Text -> Either String Puzzle
parsePuzzle = ATT.parseOnly parse
    where
        parseLine = do
            x1 <- ATT.decimal
            void (ATT.char ',')
            y1 <- ATT.decimal
            void (ATT.string " -> ")
            x2 <- ATT.decimal
            void (ATT.char ',')
            y2 <- ATT.decimal
            return (x1, y1, x2, y2)
        parse = Puzzle <$> AT.sepBy1 parseLine (ATT.char '\n')

matches :: Point -> Rec -> Bool
matches (x, y) (x1, y1, x2, y2)
    | x == x1 && x == x2 && y >= min y1 y2 && y <= max y1 y2  = True
    | y == y1 && y == y2 && x >= min x1 x2 && x <= max x1 x2  = True
    | otherwise = False

solve1 :: Puzzle -> Int
solve1 (Puzzle recs) = lengthOf (folded . to countV . filtered (>=2)) field
    where
        maxCoord :: Int
        maxCoord = maximum1Of (folded . each) recs
        field = [(x, y) | x <- [0..maxCoord], y <- [0..maxCoord]]
        boolToInt True  = 1
        boolToInt False = 0
        countV :: Point -> Int
        countV p = sumOf (folded . to (boolToInt . matches p)) recs

matches2 :: Point -> Rec -> Bool
matches2 (x, y) (x1, y1, x2, y2)
    | x == x1 && x == x2 && y >= min y1 y2 && y <= max y1 y2  = True
    | y == y1 && y == y2 && x >= min x1 x2 && x <= max x1 x2  = True
    | (x1 /= x2 || y1 /= y2) && matchesLine = True
    | otherwise = False
    where
        mx = min x1 x2
        my = min y1 y2
        mmx = max x1 x2
        mmy = max y1 y2
        matchesLine
            | x < mx = False
            | y < my = False
            | x > mmx = False
            | y > mmy = False
            | abs (x2 - x) == abs (y2 - y) = True
            | otherwise = False

solve2 :: Puzzle -> Int
solve2 (Puzzle recs) = lengthOf (folded . to countV . filtered (>=2)) field
    where
        maxCoord :: Int
        maxCoord = maximum1Of (folded . each) recs
        field = [(x, y) | x <- [0..maxCoord], y <- [0..maxCoord]]
        boolToInt True  = 1
        boolToInt False = 0
        countV :: Point -> Int
        countV p = sumOf (folded . to (boolToInt . matches2 p)) recs
