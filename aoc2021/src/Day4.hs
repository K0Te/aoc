{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec      as AT
import qualified Data.Attoparsec.Text as ATT
import           Data.List            (scanl)
import qualified Data.Map             as M
import           Data.Maybe           (fromJust)
import qualified Data.Vector          as V
import           File                 (readLinesT)
import           Import               hiding (Down, to, (%~))
import           Prelude              (print)
import qualified RIO.Text             as T

main :: IO ()
main = do
    lines_ <- readLinesT "data/4.txt"
    print lines_
    print $ parsePuzzle (T.unlines lines_)
    print $ solve1 lines_
    print $ solve2 lines_

boardSize :: Int
boardSize = 5

data Board = Board (Vector Int) [Int]
    deriving (Eq, Show)

columns :: Fold Board (Vector Int)
columns = folding (\(Board v _) ->
    V.reverse <$>
    V.ifoldl' (\m idx elm -> M.insertWith (V.++) (idx `mod` boardSize) (V.singleton elm) m) M.empty v
    )

rows :: Fold Board (Vector Int)
rows = folding (\(Board v _) ->
    V.reverse <$>
    V.ifoldl' (\m idx elm -> M.insertWith (V.++) (idx `div` boardSize) (V.singleton elm) m) M.empty v
    )

hasWin :: Board -> Bool
hasWin b@(Board _ calledNums) = hasWinRows || hasWinCols
    where
        hasWin' = allOf folded (`elem` calledNums)
        hasWinCols = anyOf columns hasWin' b
        hasWinRows = anyOf rows hasWin' b

calcScore :: Board -> Int
calcScore (Board v calledNumbers) = fromJust lastCalled * sum uncalled
    where
        lastCalled = firstOf folded calledNumbers
        uncalled = v ^.. folded . filtered (\x -> not $ x `elem` calledNumbers)

callNumber :: Int -> Board -> Board
callNumber num (Board v c)= Board v (num:c)

data Puzzle = Puzzle [Board] [Int]
    deriving (Eq, Show)

parsePuzzle :: Text -> Either String Puzzle
parsePuzzle = ATT.parseOnly parse
    where
        parseBoard = do
            rows' <- AT.manyTill (AT.many' (ATT.char '\n')  *>
                AT.many' (ATT.char ' ') *>
                AT.sepBy1 ATT.decimal (AT.many1 (ATT.char ' '))) (ATT.string "\n\n")
            return $ concat rows'
        mkBoard v = Board (V.fromList v) []
        parse = do
            calledNums <- AT.sepBy1 ATT.decimal  (ATT.char ',')
            void $ ATT.char '\n'
            void $ ATT.char '\n'
            boards <- AT.many' parseBoard
            return $ Puzzle (fmap mkBoard boards) calledNums

solve1 :: [Text] -> [Int]
solve1 input = calcScore <$> boardWon
    where
        puzzle = fromRight undefined $ parsePuzzle (T.unlines input)
        puzzles (Puzzle boards nums) = scanl (\bs x -> fmap (callNumber x) bs) boards nums
        solution = firstOf (folded . filtered (anyOf folded hasWin)) (puzzles puzzle)
        boardWon = solution ^.. folded . folded . filtered hasWin

solve2 :: [Text] -> Maybe Int
solve2 input = calcScore <$> worstWhenFinallyWon
    where
        puzzle = fromRight undefined $ parsePuzzle (T.unlines input)
        puzzles (Puzzle boards nums) = scanl (\bs x -> fmap (callNumber x) bs) boards nums
        solution = firstOf (folded . filtered (\x -> lengthOf (folded . filtered (not . hasWin)) x == 1)) (puzzles puzzle)
        worstBoard = solution ^.. folded . folded . filtered (not . hasWin)
        worstTillWin [worstBoard'] (Puzzle boards nums) = scanl (flip callNumber) worstBoard' nums
        worstRes = worstTillWin worstBoard puzzle
        worstWhenFinallyWon = firstOf (folded . filtered hasWin) worstRes
