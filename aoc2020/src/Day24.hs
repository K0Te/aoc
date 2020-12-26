{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day24 where

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
import qualified RIO.Map as M
import RIO.List.Partial (minimum)
import RIO.List.Partial (maximum)
import RIO.List (iterate, cycle)
import Prelude (repeat)
import Data.Attoparsec.Text (Parser)
import Control.Applicative
import Data.List (maximumBy, foldr1)
import Data.List (sort)
import Data.List (intersperse)
import Data.List (minimumBy)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/24.txt"
    let input = snd . partitionEithers $ parseP <$> TT.lines fileData
    logInfo (displayShow input)
    logInfo (displayShow $ solve1 input)
    logInfo (displayShow $ drop 100 $ take 101 $ solve2 input)

solve1 :: [[(Int, Int)]] -> Int
solve1 xs =
    let maps = walk1 <$> xs
        combined = foldr1 (M.unionWith (+)) maps
        blackCount = length . filter odd . M.elems $ combined
    in blackCount

solve2 :: [[(Int, Int)]] -> [Int]
solve2 xs =
    let maps = walk1 <$> xs
        combined = foldr1 (M.unionWith (+)) maps
        blackOnes = const () <$> M.filter odd combined
    in M.size <$> iterate tick blackOnes

tick :: Map (Int, Int) () -> Map (Int, Int) ()
tick m =
    let minX = fst $ minimumBy (\a b -> compare (fst a) (fst b) ) (M.keys m)
        maxX = fst $ maximumBy (\a b -> compare (fst a) (fst b) ) (M.keys m)
        minY = snd $ minimumBy (\a b -> compare (snd a) (snd b) ) (M.keys m)
        maxY = snd $ maximumBy (\a b -> compare (snd a) (snd b) ) (M.keys m)
        allCoords = [(x,y) | x<-[minX-3..maxX+3], y<-[minY-3..maxY+3]]
        nextCoords = (\c -> if becomesBlack m c then (True, c) else (False,c)) <$> allCoords
    in M.fromList $ (\(_, c) -> (c, ())) <$> filter fst nextCoords 

becomesBlack :: Map (Int, Int) () -> (Int, Int) -> Bool
becomesBlack m c =
    let ifBlack = isJust $ M.lookup c m
        count = nearByCount m c
    in case (ifBlack, count) of 
        (True, 1) -> True
        (True, 2) -> True
        (False, 2) -> True
        _ -> False

nearByCount :: Map (Int, Int) () -> (Int, Int) -> Int
nearByCount m (x, y) =
    let ml (x1, y1) = case M.lookup (x1, y1) m
            of Nothing -> 0
               Just () -> 1
    in sum [
          ml (x-2, y)
        , ml (x+2, y)
        , ml (x+1, y+1)
        , ml (x+1, y-1)
        , ml (x-1, y+1)
        , ml (x-1, y-1)
    ]

walk1 :: [(Int, Int)] -> M.Map (Int,Int) Int
walk1 xs = let final = foldr up (0,0) xs in M.singleton final 1
    where up (dx, dy) (x,y) = let nxt = (x+dx, y+dy) in nxt

walk :: [(Int, Int)] -> M.Map (Int,Int) Int
walk xs = snd $ foldr up ((0,0), M.empty) xs
    where up (dx, dy) ((x,y), m) = let nxt = (x+dx, y+dy) in (nxt, M.insertWith (+) nxt 1 m)

parseP :: Text -> Either String [(Int, Int)]
parseP = PT.parseOnly (PT.many' dir)
    where
        dir = PT.choice [
              PT.string "e" >> return (2, 0)
            , PT.string "w" >> return (-2, 0)
            , PT.string "se" >> return (1, -1)
            , PT.string "sw" >> return (-1, -1)
            , PT.string "ne" >> return (1, 1)
            , PT.string "nw" >> return (-1, 1)
            ]
