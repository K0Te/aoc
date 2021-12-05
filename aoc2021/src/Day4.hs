{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 where

import           Control.Lens  hiding ((^.), (^..))
import           Data.Bits     (Bits (setBit, shift, zeroBits))
import           Data.Foldable (foldl1)
import qualified Data.Map      as M
import           File          (readLinesT)
import           Import        hiding (Down, to, (%~))
import           Prelude       (print)
import qualified RIO.Text      as T

main :: IO ()
main = do
    lines_ <- readLinesT "data/4.txt"
    print lines_


data Board = Board

hasWin :: Board -> Bool
hasWin = undefined 

calcScore :: Board -> Int
calcScore = undefined

callNumber :: Int -> Board -> Board
callNumber = undefined

data Puzzle = Puzzle [Board] [Int]

