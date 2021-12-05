{-# LANGUAGE NoImplicitPrelude #-}
module Day1 where

import           Control.Lens hiding ((^.), (^..))
import           Data.List    (zip3)
import           File         (readLinesS)
import           Import
import           Prelude      (print, tail, zipWith3)

main :: IO ()
main = do
    lines_ <- readLinesS "data/1.txt"
    print $ solve lines_

parseList :: (Read a) => [String] -> [a]
parseList = catMaybes . (readMaybe <$>)

solve :: [String] -> Int
solve = countIncreasing . parseList

countIncreasing :: [Int] -> Int
countIncreasing l = length . filter id $ zipWith (<) l (drop 1 l)

countIncreasing3 :: [Int] -> Int
countIncreasing3 l = length . filter id $ zipWith (<) sum3 (drop 1 sum3)
    where
        sum3 = zipWith3 (\x y z -> x + y + z) l (drop 1 l) (drop 2 l)

countIncreasingL :: [Int] -> Int
countIncreasingL l = lengthOf (folded . filtered (uncurry (<))) (zip l tail_ )
    where
        tail_ = l ^. _tail

countIncreasing3L :: [Int] -> Int
countIncreasing3L l = undefined
    where
        tail1 = l ^. _tail
        tail2 = tail1 ^. _tail
        zipSum = undefined
