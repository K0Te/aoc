{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day23 where

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
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.List (foldr1)
import Data.List (sort)
import Data.List (intersperse)
import Data.List (findIndex)
import Data.List (splitAt)
import qualified Data.DList as D
import qualified Data.Sequence as SQ
import GHC.IO (unsafePerformIO)
import Data.List (tail)

-- [(1,9),(2,6),(3,2),(4,5),(5,6),(6,4),(7,3),(8,1),(9,8)])
-- 92658374

main :: HasLogFunc env => RIO env ()
main = do
    let input = [5, 9, 8, 1, 6, 2, 7, 3, 4]
    let example = [3, 8, 9, 1, 2, 5, 4, 6, 7] ++ [10..10^6]
        sq = SQ.fromList example
        mkMap l = (head l, IM.fromList $ zip l (tail l ++ [head l]))
        mapExample = mkMap example
        ex2 = [3, 8, 9, 1, 2, 5, 4, 6, 7]
        inputLong = [5, 9, 8, 1, 6, 2, 7, 3, 4] ++ [10..10^6]
    logInfo (displayShow $ stepMany 100 step input)
    logInfo (displayShow $ stepMany 100 stepM (mkMap input))
    -- logInfo (displayShow $ take 100 $ stepMany 20 step example)
    logInfo (displayShow $ solve2 $ stepMany 20 stepSQ sq)
    -- logInfo (displayShow $ solve2 $ stepMany 100000 stepSQ sq)
    -- logInfo (displayShow $ solve2 $ stepMany 200000 stepSQ sq)
    -- logInfo (displayShow $ solve3 <$> stepMany (10^7) stepM mapExample)
    logInfo (displayShow $ solve3 <$> stepMany (10^7) stepM (mkMap inputLong))

solve2 :: Seq Int -> Seq Int 
solve2 = SQ.take 10 . SQ.dropWhileL (/=1)

solve3 :: IntMap Int -> [Int]
solve3 m = fromJust $ do
    x <- IM.lookup 1 m
    y <- IM.lookup x m
    return [x,y]

stepMany :: Int -> (a->a) -> a -> a
stepMany 0 f r = r
stepMany x f r = stepMany (x-1) f (f r)

step :: [Int] -> [Int]
step (x:xs) =
    let pickUp = take 3 xs
        rest = drop 3 xs
        rot (-1) = 10^6
        rot 0 = 10^6
        rot x = x
        nexts v lst = case findIndex (==v) lst
            of Nothing -> nexts (rot (v-1)) lst
               Just nv -> splitAt (nv+1) lst
        (l, r) = nexts (x-1) rest
        res = D.fromList l `D.append` D.fromList pickUp `D.append` D.fromList r
    in D.toList $ res `D.snoc` x

findRace :: SQ.Seq Int -> Int -> Maybe Int 
findRace lst v =
    let
    l = SQ.findIndexL (==v) lst
    r = SQ.findIndexR (==v) lst
    action = race (return l) (return r)
    in unsafePerformIO $ action >>= return . either   id id

stepSQ :: SQ.Seq Int -> SQ.Seq Int
stepSQ (x SQ.:<| xs) =
    let pickUp = SQ.take 3 xs
        rest = SQ.drop 3 xs
        rot (-1) = 10^6
        rot 0 = 10^6
        rot x = x
        nexts v lst = case findRace lst v
            of Nothing -> nexts (rot (v-1)) lst
               Just nv -> SQ.splitAt (nv+1) lst
        (l, r) = nexts (x-1) rest
        res = l SQ.>< pickUp SQ.>< r
    in res SQ.|> x

stepM :: (Int, IM.IntMap Int) -> (Int, IM.IntMap Int)
stepM (c, m) =
    let (n1, n2, n3) = fromJust $ do
            n1 <- IM.lookup c m
            n2 <- IM.lookup n1 m
            n3 <- IM.lookup n2 m
            return (n1, n2, n3)
        rest = IM.delete n3 $ IM.delete n2 $ IM.delete n1 m
        rot (-1) = 10^6
        rot 0 = 10^6
        rot x = x
        nexts v lst = case IM.lookup v rest
            of Nothing -> nexts (rot (v-1)) lst
               Just nv -> v
        insAfter = nexts (c-1) rest
        r1 = IM.insert insAfter n1 rest
        r2 = IM.insert n1 n2 r1
        r3 = IM.insert n2 n3 r2
        r4 = IM.insert n3 (fromJust $ IM.lookup insAfter m) r3
        r5 = IM.insert c (fromJust $ IM.lookup n3 m) r4
    in (fromJust $ IM.lookup n3 m, r5)
