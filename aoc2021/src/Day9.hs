{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module Day9 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec      as AT
import qualified Data.Attoparsec.Text as ATT
import           Data.List            (scanl, (!!), head, sort)
import qualified Data.Set             as S
import           Data.Maybe           (fromJust)
import qualified Data.Vector          as V
import           File                 (readLinesT)
import           Import               hiding (Down, to, (%~))
import           Prelude              (print)
import qualified RIO.Text             as T
import RIO.Partial (read)
import qualified Data.Attoparsec.ByteString.Char8 as AT
import qualified Control.Monad.State as ST
import qualified Control.Monad as M


data Puzzle = Puzzle (Vector Int) Int
    deriving (Eq, Show)

parsePuzzle :: Text -> Either String Puzzle
parsePuzzle = ATT.parseOnly parse
    where
        parseLine = AT.many1 singleNum
        singleNum = read . pure <$> ATT.digit
        parse = do
            lines <- AT.sepBy1 parseLine (ATT.char '\n')
            return $ Puzzle (V.fromList . concat $ lines) (length (head lines ))

withNeighboors :: Fold Puzzle (Int, [Int])
withNeighboors = folding puzleFold
    where
        puzleFold :: Puzzle -> Vector (Int, [Int])
        puzleFold (Puzzle v size) = withIndex
            where
                withIndex = iover itraversed (\pos value -> (value, neighboors size v pos)) v
        neighboors :: Int -> V.Vector Int -> Int -> [Int]
        neighboors size v pos = let
            left = case pos `mod` size of
                0 -> Nothing 
                _ -> Just $ v V.! (pos - 1)
            right = case (pos + 1) `mod` size of
                0 -> Nothing
                _ -> Just $ v V.! (pos + 1)
            up = if pos < size then Nothing else Just $ v V.! (pos - size)
            down = v V.!? (pos + size)
            in catMaybes [left, right, up, down] 

solve1 :: Puzzle -> Int
solve1 puzzle = sumOf folded riskLevels
    where
        smaller = puzzle ^.. withNeighboors . filtered (\(val, ns) -> allOf folded (val<) ns) 
        riskLevels = smaller ^.. folded . to fst . to (+1)


--- P2
-- This algo is somehow simpler in imperative implementation :'(
-- for each point try to grow it, collect all resulting basins
data Basin = Basin { _basinPoints :: S.Set Int } deriving (Show, Eq, Ord)

makeLenses ''Basin

neighboors' :: Int -> V.Vector Int -> Int -> [Int]
neighboors' size v pos = let
    left = case pos `mod` size of
        0 -> Nothing 
        _ -> Just $ (pos - 1)
    right = case (pos + 1) `mod` size of
        0 -> Nothing
        _ -> Just $ (pos + 1)
    up = if pos < size then Nothing else Just $ (pos - size)
    down = if (pos + size) >= V.length v then Nothing else Just $ pos + size
    in catMaybes [left, right, up, down] 


growBasinS :: Puzzle -> Int -> ST.State Basin ()
growBasinS (Puzzle v size) coord = do
    ST.put (Basin $ S.singleton coord)
    let goto = do
        basin <- ST.get
        let newPoints = (concat $ (neighboors' size v <$> basin ^.. basinPoints . folded)) ^.. folded . filtered (\x -> v V.! x < 9)
            newBasin = Basin $ S.union (basin ^. basinPoints) (S.fromList newPoints)
        ST.put newBasin
        if newBasin == basin then return () else goto
    goto

-- solve2 :: Puzzle -> Int 
solve2 puzzle@(Puzzle v size) = productOf (folded) (take 3 . reverse . sort $ basinSizes)
    -- (S.fromList allBasins)
    where
        basinsS = growBasinS puzzle <$> ([0..V.length v - 1] ^.. folded . filtered (\x -> v V.! x < 9))
        allBasins = fmap (\v -> ST.execState v undefined) basinsS
        basinSizes = (S.fromList allBasins) ^.. (folded . basinPoints . to length)


main :: IO ()
main = do
    lines_ <- readLinesT "data/9.txt"
    print lines_
    let mPuzzle = parsePuzzle (T.unlines lines_)
    print $ parsePuzzle (T.unlines lines_)
    print $ fmap (toListOf withNeighboors) $ parsePuzzle (T.unlines lines_)
    print $ solve1 <$> mPuzzle
    print $ solve2 <$> mPuzzle