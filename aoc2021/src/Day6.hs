{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec      as AT
import qualified Data.Attoparsec.Text as ATT
import           File                 (readLinesT)
import           Import               hiding (Down, to, (%~))
import           Prelude              (print)
import qualified RIO.Text             as T
import Data.List ((!!))

main :: IO ()
main = do
    lines_ <- readLinesT "data/6.txt"
    print lines_
    print $ parsePuzzle (T.unlines lines_)
    print $ solve1 80 <$> parsePuzzle (T.unlines lines_)
    print $ solve1 256 <$> parsePuzzle (T.unlines lines_)

lst :: [Int]
--    8:7:6:5:4:3:2:1:0
lst = 1:1:1:1:1:1:1:1:1: zipWith (+) lst (drop 2 lst)

parsePuzzle :: Text -> Either String [Int]
parsePuzzle = ATT.parseOnly parse
    where
        parse = AT.sepBy1 ATT.decimal  (ATT.char ',')

solve1 :: Int -> [Int] -> Int
solve1 days = sum . fmap calc
    where
        calc n = lst !! (days + 8 - n)