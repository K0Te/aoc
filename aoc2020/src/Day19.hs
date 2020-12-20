{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day19 where

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

data Rule = Letter Char | Or Rule Rule | AndThen Rule Rule | RuleRef Int deriving (Eq, Show)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/19.txt"
    let [rules, dat] = TT.splitOn "\n\n"  fileData
        parsedRules = snd . partitionEithers $ parseRule <$> (TT.lines  rules)
        parsedData = TT.unpack <$> TT.lines dat
        ruleMap = M.fromList parsedRules
        rule0 = fromJust $ M.lookup 0 ruleMap
        matches = (\s -> matchRule ruleMap s rule0) <$> parsedData
        matches2 = (\s -> matchBRule ruleMap s rule0) <$> parsedData
    logInfo (displayShow parsedRules)
    logInfo (displayShow matches)
    logInfo (displayShow $ length $ filter (\(s,x) -> s=="" && x) matches)
    logInfo (displayShow $ length $ filter (any (\(s,x)-> s=="" && x)) matches2)

parseRule :: Text -> Either String (Int, Rule)
parseRule = PT.parseOnly pRule
    where
        pRule = do
            ref <- PT.decimal 
            PT.string ": "
            rule <- rule'
            return (ref, rule)
        rule' = PT.choice [
                  do
                      r1 <- rule''
                      PT.string " | "
                      r2 <- rule''
                      return $ Or r1 r2,
                  rule''
                ]
        rule'' = do
            rules <- PT.sepBy1' rule''' PT.space 
            return $ case rules of
                [r] -> r
                [r1, r2] -> AndThen r1 r2
                [r1, r2, r3] -> AndThen (AndThen r1 r2) r3
        rule''' = do
            PT.choice [
                PT.decimal  >>= return . RuleRef
                , do
                    PT.char '"'
                    x <- PT.anyChar 
                    PT.char '"'
                    return $ Letter x
                ]

-- Letter Char | Or Rule Rule | AndThen Rule Rule | RuleRef Int 
matchRule :: M.Map Int Rule -> String -> Rule -> (String, Bool)
matchRule m [] (Letter x) = ("", False)
matchRule m (s:ss) (Letter x) = (ss, x==s)
matchRule m s (Or r1 r2) =
    let (s1, res) = matchRule m s r1
    in if res then (s1, res) else matchRule m s r2
matchRule m s (AndThen r1 r2) =
    let (s1, res) = matchRule m s r1
    in if not res then (s1, res) else matchRule m s1 r2
matchRule m s (RuleRef r1) =
    let r = fromJust $ M.lookup r1 m
    in matchRule m s r

matchBRule :: M.Map Int Rule -> String -> Rule -> [(String, Bool)]
matchBRule m [] (Letter x) = [("", False)]
matchBRule m (s:ss) (Letter x) = [(ss, x==s)]
matchBRule m s (Or r1 r2) =
    let ress1 = matchBRule m s r1
        ress2 = matchBRule m s r2
    in ress1 ++ ress2
matchBRule m s (AndThen r1 r2) =
    let ress = matchBRule m s r1
    in concat $ (\(s1, res) -> if not res then [(s1, res)] else matchBRule m s1 r2) <$> ress 
matchBRule m s (RuleRef r1) =
    let r = fromJust $ M.lookup r1 m
    in matchBRule m s r