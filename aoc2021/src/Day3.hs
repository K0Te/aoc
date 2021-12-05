{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec.Text as PT
import           File                 (readLinesT)
import           Import               hiding (Down, (%~), to)
import           Prelude              (print)
import qualified RIO.Text as T
import Data.Bits (Bits(setBit, shift, zeroBits))
import qualified Data.Map as M
import Data.Foldable (foldl1)

main :: IO ()
main = do
    lines_ <- readLinesT "data/3.txt"
    print $ parseList lines_
    print $ foldl1 sumStats $ parseList lines_
    print $ getNumbers (foldl1 sumStats $ parseList lines_) ((length lines_) `div` 2)

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
charToInt _ = error "Wrong char"

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
                        _ -> error "No parse"
            in go res (T.drop 1 t)
        