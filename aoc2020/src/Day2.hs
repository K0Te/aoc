{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified Data.Attoparsec.Text as PT


main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/2.txt"
    let input = snd . partitionEithers $ (parsePassword) <$> (T.lines fileData) 
    logInfo (displayShow $ solve1 input)
    logInfo (displayShow $ solve2 input)

data Password = Password {minCount :: Int, maxCount :: Int, char :: Char, password::String} deriving (Eq, Show)

parsePassword :: Text -> Either String Password
parsePassword s = PT.parseOnly parser s
    where parser = do
            minCount <- PT.decimal
            _ <- PT.char '-'
            maxCount <- PT.decimal
            _ <- PT.space
            char <- PT.anyChar
            _ <-  PT.string ": "
            password <- PT.many' PT.anyChar
            return Password{..}

isValid1 :: Password -> Bool
isValid1 Password{..} = let count = length $ filter (== char) password in count >= minCount && count <= maxCount

solve1 :: [Password] -> Int
solve1 = length . (filter isValid1)

isValid2 :: Password -> Bool
isValid2 Password{..} = let
    indexedPassword = zip [1::Int ..] password
    matchCount = length $ filter (\(i, c) -> c == char && (i == minCount || i == maxCount)) indexedPassword
    in matchCount == 1

solve2 :: [Password] -> Int
solve2 = length . (filter isValid2)
