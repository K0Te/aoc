{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day15 where

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
import RIO.State
import Data.STRef
import Data.List (subsequences)
import Data.List (last)

data Mask = Mask {andMask:: Int, orMask :: Int, fMask:: Int} deriving (Eq, Show)
data Cmd = MaskCmd Mask | MemCmd Int Int deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/14.txt"
    let input = [1,20,8,12,0,14]
        result = numbers input
    logInfo (displayShow $ result !! 2019)
    logInfo (displayShow $ result !! (3 * 10 ^7 - 1))
    -- logInfo (displayShow $ solve2 $ run parsed)
    -- logInfo (displayShow $ solve2 parsed)
    return ()

numbers :: [Int] -> [Int]
numbers inp =
    let numbered = zip [0..] inp
        (firstRes, firstState) = runState (forM numbered (uncurry runSingnle)) M.empty
        nextInput = last firstRes
        (nextRes, nextS) = runState (
            do
                let next = [(length numbered)..]
                    go :: [Int] -> Int -> StateT (Map Int Int) Identity [Int]
                    go (nm:nms) n = runSingnle nm n >>= (\r -> (n :) <$> go nms r)
                res <- go next nextInput
                return res
            ) firstState
    in inp ++ nextRes

runSingnle :: Int -> Int -> StateT (Map Int Int) Identity Int
runSingnle stepNum x = do
    st <- get
    let r = case M.lookup x st of
            Nothing -> 0
            Just prevStep -> stepNum - prevStep 
    put $ M.insert x stepNum st
    return r
