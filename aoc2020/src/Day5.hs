{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified Data.Attoparsec.Text as PT
import Data.List (cycle)
import RIO.List.Partial ((!!))
import Data.List (head)

data Point = Tree | Empty deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/5.txt"
    let input = (fmap) parsePlaceId (T.unpack <$> T.lines fileData) 
    logInfo (displayShow input)
    logInfo (displayShow  $ solve1 input)
    logInfo (displayShow  $ solve2 input)

parsePlaceId :: String -> Int
parsePlaceId s = rowValue * 8 + calValue
    where
        row = take 7 s
        col = drop 7 s
        rowValue = parseBinary ('B', 'F') row
        calValue = parseBinary ('R', 'L') col

parseBinary :: (Char, Char) -> String -> Int
parseBinary (one, zero) s =  snd $ foldr (\v (coef, total) -> if v == one then (coef*2, total+coef) else (coef*2, total)) (1, 0) s

solve1 = foldl' max 0

solve2 :: [Int] -> [Int]
solve2 input = filter (\x -> not $ x `elem` input) [1..944]
