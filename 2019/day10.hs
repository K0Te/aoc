{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.List
import Data.List.Split
import Data.String.QQ
import Data.Maybe
import Data.Set
import Data.Fixed (mod')

x = [s|
.#.####..#.#...#...##..#.#.##.
..#####.##..#..##....#..#...#.
......#.......##.##.#....##..#
..#..##..#.###.....#.#..###.#.
..#..#..##..#.#.##..###.......
...##....#.##.#.#..##.##.#...#
.##...#.#.##..#.#........#.#..
.##...##.##..#.#.##.#.#.#.##.#
#..##....#...###.#..##.#...##.
.###.###..##......#..#...###.#
.#..#.####.#..#....#.##..#.#.#
..#...#..#.#######....###.....
####..#.#.#...##...##....#..##
##..#.##.#.#..##.###.#.##.##..
..#.........#.#.#.#.......#..#
...##.#.....#.#.##........#..#
##..###.....#.............#.##
.#...#....#..####.#.#......##.
..#..##..###...#.....#...##..#
...####..#.#.##..#....#.#.....
####.#####.#.#....#.#....##.#.
#.#..#......#.........##..#.#.
#....##.....#........#..##.##.
.###.##...##..#.##.#.#...#.#.#
##.###....##....#.#.....#.###.
..#...#......#........####..#.
#....#.###.##.#...#.#.#.#.....
.........##....#...#.....#..##
###....#.........#..#..#.#.#..
##...#...###.#..#.###....#.##.
|]

-- x = [s|
-- .#..#
-- .....
-- #####
-- ....#
-- ...##
-- |]

-- P1 (286,(22,25))

res = (fmap.fmap) (zip [0,1::Int ..]) (zip [0,1::Int ..] (lines x))
coords = catMaybes $ concat $ fmap (\(y, lst) -> fmap (\(x, v) -> if v == '.' then Nothing else Just (x,y)) lst) res
coordsSet = fromList coords
maxX = (maximum $ fst <$> coords)
maxY = (maximum $ snd <$> coords)
coord_to_paths (x,y) = do
  let vector_metric v = (abs $ fst v) + (abs $ snd v)
  (dx, dy) <- sortOn vector_metric [(dx, dy) | dx <- [-30..30], dy <- [-30..30], (not $ dx == 0 && dy == 0)]
  return [(x+dx*d, y+dy*d) | d <- [1,2..30], x+dx*d <= maxX, x+dx*d >=0, y+dy*d >=0, y+dy*d <= maxY]

count_from_coord coord = Data.List.foldl' (\(cnt, seen) path -> if sees path seen then (cnt+1, Data.Set.union seen (fromList path))  else (cnt, seen)) (0, empty) paths
  where
    paths = coord_to_paths coord
    sees path seen = any (\c -> c `member` coordsSet && (not $ c `member` seen)) path

main = do
  print $ sort (zip (fst . count_from_coord <$> coords) coords)
  let paths = coord_to_paths (22,25)
    paths2 = filter (not . Data.List.null) paths
    isUnique [] = []
    isUnique (x:xs) = (all (\el -> not $ el `elem` (concat xs)) x ) : (isUnique xs)
    uniquePaths = snd <$> (Data.List.filter (fst) $ zip (isUnique (sortOn length  paths2)) (sortOn length  paths2))
    vecs = head <$> uniquePaths
    coordToVec (x,y) = (x-22, y-25)
    uniqueVecs = coordToVec  <$>  vecs
    angles = (uncurry atan2) <$> (fmap (\(x,y) -> (fromIntegral x, fromIntegral y)) uniqueVecs)
    orderedPaths = snd <$> (reverse $ sort $ zip (fmap (\x -> x  ) angles) uniquePaths)
    pathHit path coords n =
      let hits = snd <$> (Data.List.filter (\x -> fst x) $ (fmap (\x -> (x `member` coords, x))) path) in if (Data.List.null hits) then (coords, n) else (coords Data.Set.\\ (singleton (head hits)), n+1)
    xxx  = Data.List.scanl (\(coords, n) path -> pathHit path coords n) (coordsSet,0) (cycle orderedPaths)
    res = snd <$> (Data.List.take 999 xxx)
    res2 =  Data.List.filter (\x -> ((snd x) == 199) ||  ((snd x) == 200)) res
    x1 = fst (res2 !! 5)
    x2 = fst (res2 !! 4)
    final = x2 `difference` x1
