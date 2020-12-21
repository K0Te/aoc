{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day20 where

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
import qualified Data.Vector as V
import qualified RIO.Map as M
import RIO.List.Partial (minimum)
import RIO.List.Partial (maximum)
import RIO.List (zip3, cycle)
import Prelude (repeat)
import Data.Attoparsec.Text (Parser)
import Data.Vector (ifilter)
import qualified Data.Matrix as MX
import Data.Matrix ((<|>), (<->))
import Data.List.Split (chunksOf)
import RIO.List.Partial (foldr1)

data Tile = Tile {number :: Int, content :: Vector Bool} deriving (Eq,Show, Ord)
data Side = L | R | T | B deriving (Eq, Show, Ord)
type SideR = (Int, Side, Bool)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/20.txt"
    let tiles = TT.splitOn "\n\n"  fileData
        parsedTiles = snd . partitionEithers $ parseTile <$> tiles
        allSides = concat $ sides <$> parsedTiles
        vv = [(s1, s2) | (s1, v1) <- allSides, (s2,v2) <- allSides , s1 /= s2, v1 == v2]
        -- topDown = (3539,B,False)
        -- topDown = (3709,B,True)
        topDown = (2693,L,True)
        -- topDown = (1549,L,True)
        left = connect vv S.empty topDown
        -- left = connect vv S.empty (2971,B,False)
        top = opposite (botToRight topDown) : connect vv S.empty (botToRight topDown)
        -- top = opposite (2971,R,False) : connect vv S.empty (2971,R,False)
        grid = (\x -> (opposite $ botToRight $ opposite x) : connect  vv S.empty (botToRight $ opposite x)) <$> left
        -- grid2 = (\x -> (botToRight x False) : connect  vv S.empty (opposite $ botToRight x False)) <$> left
        -- grid = zipWith (\l1 l2 -> if length l1 == 1 then l2 else l1)  grid1 grid2
        fullGrid = top : grid
        fullM = (fmap.fmap) (\ss@(n,s,f) -> contentToPos (content $ head $ filter (\x -> n == number x) parsedTiles ) ss) fullGrid
        cropped =  (fmap.fmap) (MX.submatrix 2 9 2 9) fullM
        horizontal = (foldr1 (\x y -> x Data.Matrix.<|> y)) <$> cropped
        final = foldr1 (\x y -> x Data.Matrix.<-> y) horizontal
    -- logInfo (displayShow parsedTiles)
    logInfo (displayShow $ sum $ check (final))
    logInfo (displayShow $ sum $ check (rotateC final))
    logInfo (displayShow $ sum $ check (rotateC  $ rotateC final))
    logInfo (displayShow $ sum $ check (rotateC  $ rotateC  $ rotateC final))
    logInfo (displayShow $ sum $ check (flipR final))
    logInfo (displayShow $ sum $ check (rotateC $ flipR final))
    logInfo (displayShow $ sum $ check (rotateC  $ rotateC $ flipR final))
    logInfo (displayShow $ sum $ check (rotateC  $ rotateC  $ rotateC  $ flipR final))
    logInfo (displayShow $ sum $ check (flipV final))
    logInfo (displayShow $ sum $ check (rotateC $ flipV final))
    logInfo (displayShow $ sum $ check (rotateC  $ rotateC $ flipV final))
    logInfo (displayShow $ sum $ check (rotateC  $ rotateC  $ rotateC  $ flipV final))


check :: MX.Matrix Bool -> [Int]
check mx =
    let fl = MX.toLists mx 
        zipped = zip3 fl  (drop 1 fl) (drop 2 fl) 
    in scanLines <$> zipped

scanLines :: ([Bool], [Bool], [Bool]) -> Int
scanLines (l1, l2, l3) = go (False, False, False, False) (zip3 l1 l2 l3) 0
    where
        go _ [] n = n
        go (False, _, _, _) ((_, True, _):(_,_,True):xs) n = go (True, False, False, False) (drop 1 xs) n
        go (True, False, _, _) ((_,_,True):(_,True,_):(_,True,_):(_, _, True):xs) n = go (True, True, False, False) (drop 1 xs) n
        go (True, True, False, _) ((_,_,True):(_,True,_):(_,True,_):(_, _, True):xs) n = go (True, True, True, False) (drop 1 xs) n
        go (True, True, True, False) ((_,_,True):(_,True,_):(True,True,_):(_, True,_):xs) n = go (False, False, False, False) (xs) (n+1)
        go p (x:xs) n = go p xs n


rotateC :: MX.Matrix Bool -> MX.Matrix Bool
rotateC mx =
    let dim = MX.nrows mx
        coords =  fmap (\(x,y) -> MX.fromLists [[x], [y]]) $ fmap fst  $ filter snd  $  MX.toList $  MX.mapPos (\(x, y) v -> ((x,y), v)) mx
        rotated = (\c -> MX.fromLists  [[0,1], [-1,0]] `MX.multStd` c ) <$> coords
    in MX.matrix dim dim (\(r,c) -> MX.fromLists [[r], [c-dim-1]] `elem` rotated)

flipR :: MX.Matrix Bool -> MX.Matrix Bool
flipR = MX.fromLists . reverse .  MX.toLists

flipV :: MX.Matrix Bool -> MX.Matrix Bool
flipV = MX.fromLists . (fmap reverse) .  MX.toLists

contentToPos :: Vector Bool -> SideR -> MX.Matrix Bool
contentToPos v (n, s, f) =
    let mx = MX.fromLists $  chunksOf 10 (V.toList v)
    in case (s, f) of
        (L, False) -> mx
        (B, False) -> rotateC mx
        (R, True) -> (rotateC.rotateC) mx
        (T, True) -> (rotateC.rotateC.rotateC) mx
        (L, True) -> flipR mx
        (T, False) -> (rotateC.flipR) mx
        (R, False) -> (rotateC.rotateC.flipR) mx
        (B, True) -> (rotateC.rotateC.rotateC.flipR) mx

-- r =  groupBy (\((a,b,c), _) ((a1,b1,c1), _) -> a==a1) $ sort vv
-- filter (\x -> 4 == length  x)  r
-- λ → corners = filter (\x -> 4 == length  x)  r
-- corners :: [[((Int, Side, Bool), SideR)]]
-- (0.00 secs, 510,696 bytes)
-- λ → corners !! 0
-- [((1549,L,False),(3779,R,True)),((1549,L,True),(3779,R,False)),((1549,T,False),(1297,L,True)),((1549,T,True),(1297,L,False))]
-- it :: [((Int, Side, Bool), SideR)]
-- (0.00 secs, 610,312 bytes)
-- λ → corners !! 1
-- [((2693,L,False),(1553,L,False)),((2693,L,True),(1553,L,True)),((2693,T,False),(1571,T,False)),((2693,T,True),(1571,T,True))]
-- it :: [((Int, Side, Bool), SideR)]
-- (0.01 secs, 612,592 bytes)
-- λ → corners !! 2
-- [((3539,R,False),(2357,R,False)),((3539,R,True),(2357,R,True)),((3539,B,False),(2719,B,True)),((3539,B,True),(2719,B,False))]
-- it :: [((Int, Side, Bool), SideR)]
-- (0.01 secs, 610,792 bytes)
-- λ → corners !! 3
-- [((3709,L,False),(1409,R,False)),((3709,L,True),(1409,R,True)),((3709,B,False),(3203,L,True)),((3709,B,True),(3203,L,False))]
-- it :: [((Int, Side, Bool), SideR)]
-- (0.01 secs, 608,992 bytes)
-- λ → 1549 * 2693 * 3539 * 3709
-- 54755174472007

opposite :: SideR -> SideR
opposite (x, R, f) = (x, L, f)
opposite (x, L, f) = (x, R, f)
opposite (x, T, f) = (x, B, f)
opposite (x, B, f) = (x, T, f)

botToRight :: SideR ->  SideR
botToRight (x, R, True) = (x, T, False)
botToRight (x, R, False) = (x, B, False)
botToRight (x, L, True) = (x, T, True)
botToRight (x, L, False) = (x, B, True)
botToRight (x, T, True) = (x, L, True)
botToRight (x, T, False) = (x, R, True)
botToRight (x, B, True) = (x, L, False)
botToRight (x, B, False) = (x, R, False)

connect :: [(SideR, SideR)] -> S.Set SideR -> SideR -> [SideR]
connect net seen s =
    let connected = filter (\(c,n) -> (s==c) && not (S.member n seen)) net
    in case connected of
        [(_, next)] -> next:connect net (S.insert next seen) (opposite next)
        [] -> []
        x -> error $ show x

parseTile :: Text -> Either String Tile
parseTile = PT.parseOnly pRule
    where
        pCell = do
            c <- PT.notChar '\n'
            return $ if c == '.' then False else True
        pRule = do
            PT.string "Tile "
            num <- PT.decimal
            PT.string ":\n"
            lines <- PT.sepBy1' (PT.many' pCell) (PT.char '\n')
            let d = V.fromList (concat lines)
            return Tile{number=num, content=d}

sides :: Tile -> [(SideR, Vector Bool)]
sides Tile{..} =
    let n = number
        usual = [ ((n, L , False), ifilter (\i x -> i `mod` 10 == 0) content)
                , ((n, T, False), ifilter (\i x -> i <= 9) content)
                , ((n, R, False), ifilter (\i x -> i `mod` 10 == 9) content)
                , ((n, B, False), ifilter (\i x -> i >= 90) content)] 
        rotated = (\((n, s, _), v) -> ((n, s, True), V.reverse v)) <$> usual
    in usual ++ rotated