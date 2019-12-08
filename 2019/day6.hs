module Main where

import System.IO (withFile, IOMode(..), hGetContents)
import Data.List.Split (splitOn)
import Data.Tree (unfoldTree, levels)
import Data.List (foldl', intersect)
import qualified Data.MultiMap as MM
import qualified Data.Map.Strict as M
import Data.Tuple (swap)
import Data.Maybe (fromJust)

main = do
    withFile "day6.txt" ReadMode (\handle -> do
        contents <- hGetContents handle
        print $ solve contents)

solve contents = distance -- part 1 -- orbit_count
  where
    toTup [x1, x2] = (x1, x2)
    orbit_tuples = (toTup . (splitOn ")")) <$> splitOn "\n" contents
    -- parent -> children multi-map
    mm_storage = MM.fromList orbit_tuples
    -- child -> parent map
    m_storage = M.fromList $ swap <$> orbit_tuples
    buildNode (x, mp) = let orbits = MM.lookup x mp in (x, [(child, mp) | child <- orbits])
    tree = unfoldTree buildNode ("COM", mm_storage)
    -- each childs orbit it's depth of tree (distance from COM) orbits
    orbit_count = foldl' (\(lvl, total) branch -> (lvl+1, total+(length branch)*lvl)) (0, 0) (levels tree)
    -- part 2 - distance from YOU to SAN
    path_to_com x = let parent = fromJust (M.lookup x m_storage) in if parent == "COM" then [] else parent:(path_to_com parent)
    p1 = path_to_com "YOU"
    p2 = path_to_com "SAN"
    -- same as traveling to COM and back, excluding common path
    distance = length p1 + length p2 - 2 * length (intersect p1 p2)
