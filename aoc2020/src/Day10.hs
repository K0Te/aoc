{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import Import
import RIO.Partial (fromJust, read)
import qualified RIO.Text as T
import qualified RIO.Set as S
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial ((!!), head)
import Control.Monad.ST
import Data.STRef
import Conduit as C
import Data.List (subsequences)
import RIO.List (inits)
import Data.List
import Data.List.Split

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/10.txt"
    let input = read <$> (TT.unpack <$> TT.lines fileData) :: [Int]
    logInfo (displayShow $ input)
    logInfo (displayShow $ solve1 input)
    logInfo (displayShow $ solve2 input)

solve1 :: [Int] -> Int
solve1 input = (count3 + 1) * count1
    where sorted = sort (0:input)
          diffs = zipWith (-) (drop 1 sorted) sorted
          last = head $ reverse sorted
          count1 = length $ filter (==1) diffs
          count3 = length $ filter (==3) diffs


gDiffs :: [Int] -> [Int]
gDiffs input = diffs
    where sorted = sort input
          diffs = zipWith (-) (drop 1 sorted) sorted

diffsOk :: [Int] -> Bool
diffsOk = all (<=3)

toNumbers :: (Foldable t, Num a) => t a -> [a]
toNumbers i = tail . reverse $ foldr (\v acc -> (v+head acc) : acc) [0] i

solve2 :: [Int] -> [Int]
solve2 input = combCount <$> subs
    where in2 = 0:input
          diffs = gDiffs in2
          subs = Data.List.Split.splitOn [3] diffs

combCount :: Foldable t => t Int -> Int
combCount x = S.size $ S.fromList (combinations x)

combinations :: Foldable t => t Int -> [[Int]]
combinations x = let lx = length x
    in filter (\y -> sum y == sum x)  $  filter diffsOk  $ subsequences  (take (3*lx) (cycle [1,2,3]))