{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day7 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified RIO.Map as M
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial (head)

data Rule = Rule Text [(Int, Text)] deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/7.txt"
    let input = snd . partitionEithers $ parseRule <$> (TT.lines fileData) 
    logInfo (displayShow input)
    logInfo (displayShow $ solve1 input - 1)
    logInfo (displayShow $ solve2 input - 1)

parseRule :: Text -> Either String Rule
parseRule s = PT.parseOnly parser s
    where parser = do
            color <- PT.manyTill PT.anyChar (PT.string " bags contain ")
            res <- PT.eitherP (PT.string "no other bags") parseContainedBy
            return $ Rule (T.pack color) (resToList res)
          parseContainedBy = PT.sepBy' parseBag (PT.char ',' >> PT.skipMany PT.space)
          parseBag = do
              count <- PT.decimal
              PT.space
              color <- PT.manyTill' PT.anyChar (PT.string " bag")
              PT.skipMany (PT.char 's')
              return (count, T.pack color)
          resToList (Left _) = []
          resToList (Right x) = x

shinyGold :: Text
shinyGold = "shiny gold"

solve1 :: [Rule] -> Int
solve1 rules = length $ go [shinyGold]
    where
        go visited = case visitNew visited rules of
            [] -> visited
            xs -> go $ xs ++ visited

visitNew :: [Text] -> [Rule] -> [Text]
visitNew visited rules = foldl' (\acc rule -> tryVisit rule visited ++ acc) [] rules
    where tryVisit (Rule color lst) visited = foldl' (\acc name -> if canMatch name lst && not (color `elem` visited) then color : acc else acc) [] visited

canMatch :: Text -> [(Int, Text)] ->  Bool
canMatch name rules = name `elem` rulesS
    where rulesS = snd <$> rules

solve2 :: [Rule] -> Int
solve2 = getDepth shinyGold

getDepth :: Text -> [Rule] -> Int
getDepth t rules = case findRule t rules of
    Rule _ [] -> 1
    Rule _ lst -> foldl' (\acc (v, color) -> acc + v * getDepth color rules) 1 lst

findRule :: Text -> [Rule] -> Rule
findRule t rules = head  $ filter (\(Rule c lst) -> c == t) rules
