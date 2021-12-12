{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day7 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec      as AT
import qualified Data.Attoparsec.Text as ATT
import           Data.List            (sort, (!!))
import           File                 (readLinesT)
import           Import               hiding (Down, to, (%~))
import           Prelude              (print)
import qualified RIO.Text             as T

main :: IO ()
main = do
    lines_ <- readLinesT "data/7.txt"
    print lines_
    print $ parsePuzzle (T.unlines lines_)
    print $ solve1 <$> parsePuzzle (T.unlines lines_)
    print $ solve2 <$> parsePuzzle (T.unlines lines_)

parsePuzzle :: Text -> Either String [Int]
parsePuzzle = ATT.parseOnly parse
    where
        parse = AT.sepBy1 ATT.decimal  (ATT.char ',')

solve1 :: [Int] -> Int
solve1 x = sumOf (folded . to (abs . (avgPos -))) x
    where
        avgPos = (sort x) !! (lengthOf folded x `div` 2)
-- 1 -> 1
-- 2 -> 3
-- 3 -> 5

lst :: [Int]
lst = 0:1:zipWith (+) (drop 1 lst) [2..]

solve2 :: Foldable f => f Int -> Maybe (Int, Int)
solve2 x = minimumOf folded allCosts
    where
        allPositions = [0..maximum1Of folded x]
        singleCost pos xp = sumOf folded [lst !! abs (pos -  xp)]
        calcPosCost :: Int -> Int
        calcPosCost p = sumOf (folded . to (singleCost p)) x
        allCosts = zip  (calcPosCost <$> allPositions) allPositions
