{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified RIO.Map as M
import qualified Data.Text as TT
import qualified Data.Set as S
import qualified Data.Attoparsec.Text as PT

type Group = S.Set Char

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/6.txt"
    let input = parseGroup <$> (TT.splitOn "\n" <$> TT.splitOn "\n\n" fileData) 
    logInfo (displayShow input)
    logInfo (displayShow $ solve2 input)

parseGroup :: [Text] -> Group
parseGroup groupMembers = foldl' S.intersection (S.fromList ['a'..'z']) sets
    where sets = fmap (S.fromList . T.unpack) groupMembers

-- solve1 :: [Group] -> Int
-- solve1 groups = sum $ fmap (S.size . S.fromList . T.unpack) groups

solve2 :: [Group] -> Int
solve2 = sum . (fmap S.size)
