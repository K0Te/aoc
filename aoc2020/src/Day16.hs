{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day16 where

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
import Data.Bits
import qualified RIO.Map as M
import Data.STRef
import Data.List (subsequences, repeat,  cycle)
import Data.List.Split

data Rule = Rule {name :: String, from1 :: Int, to1 :: Int, from2 :: Int, to2 :: Int} deriving Show

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/16.txt"
    let [rules, my, others] = TT.splitOn "\n\n" fileData
        pRules = snd . partitionEithers $ parseRule <$> (TT.lines rules)
        [mTicket] = snd . partitionEithers $ [parseTicket ((TT.lines my) !! 1)]
        oTickets = snd . partitionEithers $ parseTicket <$> (drop 1 $ TT.lines others)
    logInfo (displayShow $ pRules)
    logInfo (displayShow $  mTicket)
    logInfo (displayShow $  oTickets)
    logInfo (displayShow $  solve1 pRules oTickets)
    logInfo (displayShow $  solve2 pRules oTickets)

solve1 :: Foldable t => [Rule] -> t [Int] -> Int
solve1 rules = sum . filter (not . isValid rules) . concat  

parseRule :: Text -> Either String Rule
parseRule s = PT.parseOnly parse s
    where parse = do
            name <- PT.manyTill PT.anyChar (PT.char ':')
            PT.space
            f1 <- PT.decimal
            PT.char '-'
            t1 <- PT.decimal
            PT.string " or "
            f2 <- PT.decimal
            PT.char '-'
            t2 <- PT.decimal
            return $ Rule {name=name, from1=f1, to1=t1, from2=f2, to2=t2}

parseTicket :: Text -> Either String [Int]
parseTicket s = PT.parseOnly parse s
    where parse = PT.sepBy' PT.decimal (PT.char ',') 

isValid :: [Rule] -> Int -> Bool
isValid rs x = any (\r -> ruleValid r x) rs


ruleValid :: Rule -> Int -> Bool
ruleValid Rule{..} x = (from1 <= x && x <= to1) || (from2 <= x && x <= to2)

excludeInvalid :: [Rule] -> [[Int]] -> [[Int]]
excludeInvalid rs = filter (all (isValid rs))

solve2 :: [Rule] -> [[Int]] -> [[Int]]
solve2 rules inp =
    let filtered = excludeInvalid rules inp
        validForPositions = foldl' (\acc r -> matchPositions r:acc) [] rules
        matchPositions r = fst <$> foldl' (\acc ticket -> acc ++ filter (\x -> ruleValid r (snd x)) (zip [1..] ticket))  [] filtered 
        matchingRule poss = foldl' (\acc p -> if (length filtered) == (length $ filter (==p) poss) then p:acc else acc) [] [1..length rules]
    in matchingRule <$> validForPositions

-- [[19,16,9,8,5,3,1]
-- [16]
-- [20,19,18,17,16,12,11,10,9,8,7,6,5,4,3,2,1]
-- [20,19,18,17,16,13,12,11,10,9,8,7,6,5,4,3,2,1],
-- [20,19,18,17,16,12,11,9,8,7,6,5,4,3,1]
-- [16,9,8,3]
-- [20,19,18,17,16,12,11,9,8,7,6,5,3,1]
-- [16,9,8,5,3]
-- [16,9,8]
-- [20,19,18,17,16,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
-- [20,19,18,17,16,12,11,9,8,7,6,5,4,3,2,1]
-- [16,8]
-- [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
-- [16,9,8,5,3,1]
-- [20,19,17,16,9,8,5,3,1]
-- [20,19,17,16,12,11,9,8,7,6,5,3,1]
-- [20,19,17,16,12,11,9,8,6,5,3,1]
-- [20,19,17,16,11,9,8,6,5,3,1]
-- [20,19,17,16,11,9,8,5,3,1],[20,19,16,9,8,5,3,1]]

---- [19]
-- [16]
-- [10]
-- [13]
-- [4]
-- [3]
-- [18]
-- [5]
-- [9]
-- [14]
-- [2]
-- [8]
-- [15]
-- [1]
-- [17]
-- [7]
-- [12]
-- [6]
-- [11]
-- [20]

-- refs = [19, 16, 10, 13, 4, 3]
-- λ → sum $ (\x -> mTicket !! x) <$>  [19, 16, 10, 13, 4, 3]
-- 758

-- [8]
-- [15]
-- [1]
-- [17,7,12,6],11,20]