{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day22 where

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
import RIO.List (cycle)
import Prelude (repeat)
import Data.Attoparsec.Text (Parser)
import Control.Applicative
import Data.List (foldr1)
import Data.List (sort)
import Data.List (intersperse)

main :: HasLogFunc env => RIO env ()
main = do
    let p1 = [43, 21, 2, 20, 36, 31, 32, 37, 38, 26, 48, 47, 17, 16, 42, 12, 45, 19, 23, 14, 50, 44, 29, 34, 1]
        p2 = [40, 24, 49, 10, 22, 35, 28, 46, 7, 41, 15, 5, 39, 33, 11, 8, 3, 18, 4, 13, 6, 25, 30, 27, 9]
    logInfo (displayShow $ play p1 p2)
    logInfo (displayShow $ solve1 $ play p1 p2)
    logInfo (displayShow $ play2 p1 p2 S.empty)
    logInfo (displayShow $ solve1 . snd $ play2 p1 p2 S.empty)

playStep :: Ord a => [a] -> [a] -> ([a], [a])
playStep (x:xs) (y:ys) = if x > y then (xs++[x,y], ys) else (xs, ys ++ [y,x])

play :: Ord a => [a] -> [a] -> [a]
play [] ys = ys
play xs [] = xs
play xs ys = uncurry play (playStep xs ys)

solve1 :: [Int] -> Int 
solve1 = sum . fmap (uncurry (*)) . zip [1..] . reverse

-- P2

playStep2 :: [Int] -> [Int] -> ([Int], [Int])
playStep2 (x:xs) (y:ys) =
    let canRec = length xs >= x && length ys >= y
        res b = if b then (xs++[x,y], ys) else (xs, ys ++ [y,x])
    in if canRec
        then let (w, _) = play2 (take x xs) (take y ys) S.empty in res $ w == 1
        else res (x > y)

play2 :: [Int] -> [Int] -> Set ([Int], [Int])-> (Int, [Int])
play2 [] ys _ = (2, ys)
play2 xs [] _ = (1, xs)
play2 xs ys s =
    let (nx, ny) = playStep2 xs ys
        halt = (xs, ys) `S.member` s
    in if halt then (1, []) else play2 nx ny (S.insert (xs, ys) s)

