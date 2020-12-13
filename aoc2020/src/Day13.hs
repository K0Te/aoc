{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day13 where

import Import
import RIO.Partial (fromJust, read)
import qualified RIO.Text as T
import qualified RIO.Set as S
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial ((!!), head)
import Control.Monad.ST
import Data.Vector ((!?))
import RIO.Vector (imap)
import Data.Vector (fromList)
import GHC.List (last)
import Data.List (maximumBy)
import RIO.List (sortBy)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/13.txt"
    let [ts, line] = TT.lines fileData
        timestamp = read $ TT.unpack ts
        nums =  fmap (read . TT.unpack) . filter (/= "x") $ TT.splitOn "," line
        rules2 = parseRules $ TT.splitOn "," line
    logInfo (displayShow $ nums)
    logInfo (displayShow $ solve1 timestamp nums)
    logInfo (displayShow $ rules2)
    logInfo (displayShow $ head (solve2 rules2))
    -- logInfo (displayShow $ solve2 parsed)

solve1 :: Int -> [Int] -> (Int, Int)
solve1 ts nums =
    let timestamps = [ts..]
        matches = [(t,n) |t<-timestamps, n<-nums, t `mod` n == 0]
    in head matches

data Rule = Rule {offset :: Int, divider :: Int} deriving (Eq, Show)
parseRules :: [Text] -> [Rule]
parseRules i = snd $ foldl' (\(offset, rules) v -> if v == "x" then (offset+1, rules) else (offset+1, Rule offset (read (TT.unpack v)) : rules)) (0, []) i

matchRules :: [Rule] -> Int -> Bool
matchRules rules i = all matchRule rules
    where matchRule Rule{..} = (i+offset) `mod` divider == 0

repack :: [Rule] -> (Int, Int, [Rule])
repack r =
    let maxRule = maximumBy (\x1 x2 -> compare (divider x1) (divider x2)) r
        results = fmap (\rule -> Rule{offset=offset rule - offset maxRule, divider=divider rule}) r
    in (divider maxRule, offset maxRule, results)

solve2 :: [Rule] -> [Int]
solve2 r =
    let (inc, o, r2) = repack r
    in [(t-o) | t<- ((\x->x-92) <$> [16441*19*310571,16441*19*310572..]), matchRules r2 t]

-- [
--     (19,0),
--     (37,13),
--     (401,19),
--     (29,21),
--     (17,2),
--     (13,11),
--     (509,50),
--     (41,19),
--     (23,4)
    
-- ]

-- λ → head [x | x<-[50, 50+509..], x `mod` 401 == 19]
-- 202123
-- it :: Integral a => a
-- (0.01 secs, 618,760 bytes)
-- λ → head [x | x<-[202123, 202123+509*401..], x `mod` 41 == 19]
-- 4488412
-- it :: Integral a => a
-- (0.01 secs, 523,352 bytes)
-- λ → head [x | x<-[4488412,4488412+509*401*41 ..], x `mod` 37 == 13]
-- 163489323
-- it :: Integral a => a
-- (0.01 secs, 524,424 bytes)
-- λ → head [x | x<-[163489323, 163489323+509*401*41*37 ..], x `mod` 29 == 21]
-- 1711656088
-- it :: Integral a => a
-- (0.02 secs, 521,688 bytes)
-- λ → head [x | x<-[1711656088,1711656088+509*401*41*37*29 ..], x `mod` 23 == 4]
-- 118443430169
-- it :: Integral a => a
-- (0.01 secs, 525,320 bytes)
-- λ → head [x | x<-[118443430169,118443430169+509*401*41*37*29*23 ..], x `mod` 17 == 2]
-- 1151070662424
-- it :: Integral a => a
-- (0.01 secs, 524,120 bytes)
-- λ → head [x | x<-[1151070662424,1151070662424+509*401*41*37*29*23*17 ..], x `mod` 13 == 11]
-- 43282261738428
-- it :: Integral a => a
-- (0.01 secs, 526,760 bytes)
-- λ → head [x | x<-[43282261738428,43282261738428+509*401*41*37*29*23*17*13 ..], x `mod` 19 == 0]
-- 225850756401112
-- it :: Integral a => a
-- (0.02 secs, 525,560 bytes)
-- λ → 225850756401112 - 73
-- 225850756401039
-- it :: Num a => a
-- (0.00 secs, 522,976 bytes)
