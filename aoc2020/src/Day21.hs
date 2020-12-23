{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day21 where

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
    fileData <- readFileUtf8 "data/21.txt"
    let input = snd . partitionEithers $ parseP <$> TT.lines fileData
    logInfo (displayShow input)
    logInfo (displayShow $ solve1 input)
    logInfo (displayShow $ solve2 input)

solve1 ::  [([String], [String])] -> Int
solve1 xs =
    let alergenes = concat $ snd <$> xs
        ingrs = concat $ fst <$> xs
        uniqueAlergens = S.toList $ S.fromList alergenes
        possibleMatches = M.fromList $ (\k -> 
            let matches = filter (\x -> k `elem` snd x) xs
                matchesSet = (S.fromList . fst ) <$> matches
            in (k, foldr1 S.intersection matchesSet)) <$> uniqueAlergens
        allSuspicious = foldr1 S.union (M.elems possibleMatches)
    in foldl' (\acc v -> if S.member v allSuspicious then acc else acc+1) 0 ingrs

-- solve2 ::  [([String], [String])] -> String
solve2 xs =
    let alergenes = concat $ snd <$> xs
        ingrs = concat $ fst <$> xs
        uniqueAlergens = S.toList $ S.fromList alergenes
        possibleMatches = M.fromList $ (\k -> 
            let matches = filter (\x -> k `elem` snd x) xs
                matchesSet = (S.fromList . fst ) <$> matches
            in (k, foldr1 S.intersection matchesSet)) <$> uniqueAlergens
        matches = (\(k, v) -> (k, head $ S.toList v))  <$> M.toList possibleMatches
    -- in concat $ intersperse "," $ snd <$> sort matches 
    in possibleMatches

constP :: Parser a -> Text -> Parser a
constP p t = case PT.parseOnly p t of
  Left _ -> empty
  Right a -> return a

parseP :: Text -> Either String ([String], [String])
parseP = PT.parseOnly pRule
    where
        pRule = do
            start <- PT.takeWhile (/= '(')
            PT.string "("
            ingridients <- constP (PT.sepBy1 (PT.many1 (PT.notChar ' ')) (PT.space)) start
            PT.string "contains "
            alegens <- PT.sepBy1 (PT.many1 (PT.letter )) (PT.string ", ")
            PT.string ")"
            return (ingridients, alegens) 
