{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day8 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec      as AT
import qualified Data.Attoparsec.Text as ATT
import           File                 (readLinesT)
import           Import               hiding (Down, to, (%~))
import           Prelude              (print)
import qualified RIO.Text             as T
import qualified Data.List as L
import qualified Data.String as S

main :: IO ()
main = do
    lines_ <- readLinesT "data/8.txt"
    print lines_
    print $ parsePuzzle (T.unlines lines_)
    print $ solve1 <$> parsePuzzle (T.unlines lines_)
    print $ solve2 <$> parsePuzzle (T.unlines lines_)

newtype Puzzle = Puzzle [([Text], [Text])]
    deriving (Eq, Show)

parsePuzzle :: Text -> Either String Puzzle
parsePuzzle = ATT.parseOnly parse
    where
        parseLine = do
            variants <- ATT.sepBy (ATT.takeWhile (\c -> c /= ' ' && c /= '|') ) (ATT.char ' ')
            ATT.char '|'
            ATT.space
            input <- ATT.sepBy (ATT.takeWhile (\c -> c /= ' ' && c /= '\n') ) (ATT.char ' ')
            return (filter (/= "") variants, input)
        parse = Puzzle <$> AT.sepBy1 parseLine (ATT.char '\n')

solve1 :: Puzzle -> Int
solve1 (Puzzle recs) = lengthOf (folded . to snd . folded . to T.unpack . filtered hashUniqueMatch) recs
    where
        hashUniqueMatch :: [a] -> Bool
        hashUniqueMatch x = case lengthOf folded x of
            2 -> True -- 1
            4 -> True -- 4
            3 -> True -- 7
            7 -> True -- 8
            _ -> False

-- for each segment combination, assume that it's correct
-- check if all combos produce a valid digit
-- filter
-- produce a digit?
-- 5040 combinations for each line - not much at all

strToDigit :: String -> Maybe Int 
--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....

--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
strToDigit "abcefg" = Just 0
strToDigit "cf" = Just 1
strToDigit "acdeg" = Just 2
strToDigit "acdfg" = Just 3
strToDigit "bcdf" = Just 4
strToDigit "abdfg" = Just 5
strToDigit "abdefg" = Just 6
strToDigit "acf" = Just 7
strToDigit "abcdefg" = Just 8
strToDigit "abcdfg" = Just 9
strToDigit _ = Nothing

applyTranslation :: [String] -> [(Char, Char)] -> [String]
applyTranslation line table = L.sort . replaceChar <$> line
    where
        replaceChar :: String -> String
        replaceChar s = _repl <$> s
            where _repl c1 = maybe c1 id $ firstOf (folded . filteredBy (to fst . only c1) . to snd) table

-- solve2 :: (Show a) => Puzzle -> a
-- solve2 :: Puzzle -> [Maybe [(Char, Char)]]
solve2 (Puzzle recs) = sumOf folded (uncurry calcValue <$> solvedLines)
    where
        allTranslationTables :: [[(Char, Char)]]
        allTranslationTables = fmap (uncurry zip) $ zip (L.permutations "abcdefg") (L.repeat "abcdefg")
        lines :: [[String]]
        lines = recs ^.. folded . to fst . to (fmap T.unpack)
        firstValidTranslation :: [String] -> Maybe [(Char, Char)]
        firstValidTranslation line = firstOf (folded . filtered (allValid line)) allTranslationTables
        allValid :: [String] -> [(Char, Char)] -> Bool
        allValid line table = allOf (folded . to strToDigit . to isJust) id (applyTranslation line table)
        solvedLines = zip (firstValidTranslation <$> lines) (recs ^.. folded . to snd . to (fmap T.unpack))
        calcValue (Just table) digits = x1*1000 + x2*100 + x3*10 + x4
            where
                converted = (strToDigit  <$> (applyTranslation digits table))
                [Just x1, Just x2, Just x3, Just x4] = converted
