{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day14 where

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
import Data.List (subsequences)

data Mask = Mask {andMask:: Int, orMask :: Int, fMask:: Int} deriving (Eq, Show)
data Cmd = MaskCmd Mask | MemCmd Int Int deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/14.txt"
    let input = TT.lines fileData
        parsed = snd . partitionEithers $ parse <$> input
    logInfo (displayShow $ parsed)
    logInfo (displayShow $ run parsed)
    logInfo (displayShow $ solve1 $ run parsed)
    logInfo (displayShow $ solve1 $ run2 parsed)
    -- logInfo (displayShow $ solve2 $ run parsed)
    -- logInfo (displayShow $ solve2 parsed)

solve1 :: M.Map Int Int -> Int
solve1 x = sum $ snd <$> M.toList x

parse :: Text -> Either String Cmd
parse s = PT.parseOnly (parseMem <|> parseMask) s
    where parseMem = do
            PT.string "mem["
            addr <- PT.decimal
            PT.string "] = "
            value <- PT.decimal
            return $ MemCmd addr value
          parseMask = do
              PT.string "mask = "
              mask <- PT.many1' (PT.char 'X' <|> PT.char '0' <|> PT.char '1')
              let mask1 = foldl' (\acc v -> if v == '1' then setBit (shiftL acc 1) 0 else (shiftL acc 1)) 0 mask 
              let mask0 = foldl' (\acc v -> if v == '0' then setBit (shiftL acc 1) 0 else (shiftL acc 1)) 0 mask 
              let maskF = foldl' (\acc v -> if v == 'X' then setBit (shiftL acc 1) 0 else (shiftL acc 1)) 0 mask 
              return $ MaskCmd Mask{andMask=complement mask0, orMask=mask1, fMask=maskF}

run :: [Cmd] -> Map Int Int
run cmds = runST $ do
    memory <- newSTRef M.empty
    mask <- newSTRef Mask{orMask=zeroBits, andMask=complement zeroBits, fMask=zeroBits}
    mapM_ (\cmd -> case cmd of
                (MemCmd addr value) -> do
                    currentMask <- readSTRef mask
                    let result = (value .|. (orMask currentMask)) .&. (andMask currentMask) 
                    modifySTRef' memory (M.insert addr result)
                    return ()
                (MaskCmd nmask) -> do
                    writeSTRef mask nmask
                    return ()
        ) cmds
    readSTRef memory

fuzz :: Int -> Int -> [Int]
fuzz fzMask value =
    let indexes = filter (testBit fzMask) [0..36]
        zeroedValue = foldl' clearBit value indexes
        variants = subsequences indexes
    in (\s -> foldl' setBit zeroedValue s) <$> variants

run2 :: [Cmd] -> Map Int Int
run2 cmds = runST $ do
    memory <- newSTRef M.empty
    mask <- newSTRef Mask{orMask=zeroBits, andMask=complement zeroBits, fMask=zeroBits}
    mapM_ (\cmd -> case cmd of
                (MemCmd addr value) -> do
                    currentMask <- readSTRef mask
                    let newAddr = (addr .|. (orMask currentMask))
                    forM_ (fuzz (fMask currentMask) newAddr) (\x -> modifySTRef' memory (M.insert x value))
                    return ()
                (MaskCmd nmask) -> do
                    writeSTRef mask nmask
                    return ()
        ) cmds
    readSTRef memory
