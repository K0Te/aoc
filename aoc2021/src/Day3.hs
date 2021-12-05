{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 where

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
    lines_ <- readLinesT "data/3.txt"
    let parsedLines = parseList lines_
        cutoffCount = (length lines_) `div` 2
    print $ parsedLines
    print $ foldl1 sumStats parsedLines
    print $ getNumbers (foldl1 sumStats parsedLines) cutoffCount
    print $ getNumbers2 parsedLines

getNumbers2 :: [Stat] ->(Int, Int)
getNumbers2 stats = (oxy, co2)
    where
        oxy = constructBinary $ findNum (\value cutoff -> case value `compare` cutoff of
            LT -> 0
            EQ -> 1
            GT -> 1
            )
            stats 11
        co2 = constructBinary $ findNum (\value cutoff -> case value `compare` cutoff of
            LT -> 1
            EQ -> 0
            GT -> 0
            )
            stats 11
        findNum :: (Int -> Int -> Int) -> [Stat] -> Int -> Stat
        findNum f stats ii =
            let st = foldl1 sumStats stats
                cutoff = (lengthOf folded stats) `div` 2 + (lengthOf folded stats) `mod` 2
            in
            case lengthOf folded stats of
            1 -> stats ^. folded
            0 -> error "All filtered out!"
            _ -> traceShow ("BIT: " <> show ii) $ traceShow stats $ findNum f (stats ^.. folded . filtered
                (\v -> M.lookup ii v == ((\sv -> f sv cutoff) <$> M.lookup ii st)))
                (ii-1)
        constructBinary :: Stat -> Int
        constructBinary s =
            M.foldrWithKey (\index val bin -> if val == 1 then setBit bin index else bin) zeroBits s


getNumbers :: Stat -> Int -> (Int, Int)
getNumbers st cutoff = (gamma, eplison)
    where
        gamma = constructBinary (>cutoff)
        eplison = constructBinary (<=cutoff)
        constructBinary f =
            M.foldrWithKey (\index val bin -> if f val then setBit bin index else bin) zeroBits st

parseList :: [Text] -> [Stat]
parseList = fmap parseStat

type Stat = Map Int Int

sumStats :: Stat -> Stat -> Stat
sumStats = M.unionWith (+)

charToInt :: Char -> Int
charToInt '1' = 1
charToInt '0' = 0
charToInt _   = error "Wrong char"

parseStat :: Text -> Stat
parseStat t = M.fromList $ (t ^.. reversed . each . to charToInt) ^@.. itraversed

parseBinary :: Text -> Int
parseBinary = go 0
    where
        go :: Int -> Text -> Int
        go x "" = x
        go x t = let res = case T.take 1 t of
                        "1" -> setBit (shift x 1) 0
                        "0" -> (shift x 1)
                        _   -> error "No parse"
            in go res (T.drop 1 t)
