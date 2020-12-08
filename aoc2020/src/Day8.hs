{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day8 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified RIO.Set as S
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial ((!!), head)
import Control.Monad.ST
import Data.STRef

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/8.txt"
    let input = snd . partitionEithers $ parseCode <$> (TT.lines fileData) 
    logInfo (displayShow input)
    logInfo (displayShow $ solve1 input)
    logInfo (displayShow $ solve2 input)

data Instruction = Jmp Int | Acc Int | Noop Int deriving (Eq, Show)

parseCode :: Text -> Either String Instruction
parseCode s = PT.parseOnly parser s
    where parser = do
            code <- PT.manyTill PT.anyChar PT.space
            sign <- PT.anyChar
            val <- PT.decimal
            let inst = case code of
                    "jmp" -> Jmp argument
                    "acc" -> Acc argument
                    "nop" -> Noop argument
                argument = if sign == '-' then -val else val
            return inst

solve1 :: [Instruction] -> Int
solve1 instructions = runST $ do
    let numbered = zip instructions [1..]
    visited <- newSTRef S.empty
    acc <- newSTRef (0::Int)
    position <- newSTRef (0::Int)
    let stepVisited = do
            currentPos <- readSTRef position
            currentVisited <- readSTRef visited
            let (instruction, number) = numbered !! currentPos
                alreadyVisited = S.member number currentVisited 
            return alreadyVisited
    let runStep = do
            currentPos <- readSTRef position
            let (instruction, number) = numbered !! currentPos
            modifySTRef' visited (S.insert number)
            case instruction of
                Noop _ -> modifySTRef' position (+1)
                Jmp v -> modifySTRef' position (+v)
                Acc v -> modifySTRef' position (+1) >> modifySTRef' acc (+v)
    let go = do
            flag <- stepVisited
            unless flag $ runStep >> go
    go
    readSTRef acc


solve2 :: [Instruction] -> [(Bool, Int)]
solve2 instructions = let fuzzed = fuzzInstrucitons instructions in filter fst $ runInstructions <$> fuzzed 

fuzzInstrucitons :: [Instruction] -> [[Instruction]]
fuzzInstrucitons instructions = do
    (i, num) <- zip instructions [0..]
    let newValue = case i of
            Noop x -> Jmp x
            Jmp x -> Noop x
            Acc x -> Acc x
    return $ take (num-1) instructions ++ [newValue] ++ drop num instructions

runInstructions :: [Instruction] -> (Bool, Int)
runInstructions instructions = runST $ do
    let numbered = zip instructions [1..]
    visited <- newSTRef S.empty
    acc <- newSTRef (0::Int)
    position <- newSTRef (0::Int)
    let stepVisited = do
            currentPos <- readSTRef position
            currentVisited <- readSTRef visited
            let (instruction, number) = numbered !! currentPos
                alreadyVisited = S.member number currentVisited 
            return alreadyVisited
        runStep = do
            currentPos <- readSTRef position
            let (instruction, number) = numbered !! currentPos
            modifySTRef' visited (S.insert number)
            case instruction of
                Noop _ -> modifySTRef' position (+1)
                Jmp v -> modifySTRef' position (+v)
                Acc v -> modifySTRef' position (+1) >> modifySTRef' acc (+v)
        isLast = do
            currentPos <- readSTRef position
            return $ currentPos == length instructions
        go = do
            flag <- stepVisited
            end <- isLast
            isValid <- posValid
            when isValid $ unless (flag || end) $ runStep >> go
        posValid = do
            currentPos <- readSTRef position
            return $ currentPos >= 0 && currentPos < length instructions
    go
    end <- isLast
    res <- readSTRef acc
    return (end, res)