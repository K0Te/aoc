{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day18 where

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

data Expr = Num Int | Plus Expr Expr | Ml Expr Expr deriving (Eq, Show)

eval :: Expr -> Int
eval (Num x) = x
eval (Plus x y) = eval x + eval y
eval (Ml x y) = eval x * eval y

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/18.txt"
    let input = TT.lines fileData
        parsed = snd . partitionEithers $ parse <$> input
        parsed2 = snd . partitionEithers $ parseWithAddition <$> input
    logInfo (displayShow $ input)
    logInfo (displayShow $ parsed)
    logInfo (displayShow $ sum $ eval <$> parsed)
    logInfo (displayShow $ sum $ eval <$> parsed2)


data Token = N Int | Sum | Mul | Popen | Pclose deriving (Eq, Show)

pTokens :: Parser [Token] 
pTokens = PT.many' $ do
    _ <- PT.many' PT.space
    PT.choice [ N <$> PT.decimal,
        PT.char '+' >> return Sum,
        PT.char '*' >> return Mul,
        PT.char '(' >> return Popen,
        PT.char ')' >> return Pclose
        ]

parse :: Text -> Either String Expr
parse s = do
    tokens <- PT.parseOnly pTokens s
    let tokensToExpr (e:es) [] = e
        tokensToExpr acc ((N x):xs) = tokensToExpr ((Num x):acc) xs
        tokensToExpr (e:es) (Sum:xs) = Plus e (tokensToExpr es xs)
        tokensToExpr (e:es) (Mul:xs) = Ml e (tokensToExpr es xs)
        tokensToExpr es (Pclose:xs) =
            let (curr, nx) = takeTill 0 xs
                currE = tokensToExpr [] curr
                in tokensToExpr (currE:es) nx
        takeTill 0 (Popen:xs) = ([],xs)
        takeTill n (Popen:xs) = let (r,m) = takeTill (n-1) xs in (Popen:r, m)
        takeTill n (Pclose:xs) = let (r, m) = takeTill (n+1) xs in (Pclose:r, m)
        takeTill n (x:xs) = let (r,m) = takeTill n xs in (x:r, m)
    return $ tokensToExpr [] (reverse tokens)

parseWithAddition :: Text -> Either String Expr
parseWithAddition s = do
    tokens <- PT.parseOnly pTokens s
    let tokensToExpr (e:es) [] [] = e
        tokensToExpr acc l ((N x):xs) = tokensToExpr ((Num x):acc) l xs
        tokensToExpr es l (Mul:xs) = Ml (tokensToExpr es [] l) (tokensToExpr [] [] xs)
        tokensToExpr es l (Sum:xs) = tokensToExpr es (Sum:l) xs
        tokensToExpr (e:es) (Sum:xs) [] = Plus e (tokensToExpr es xs [])
        tokensToExpr es l (Pclose:xs) =
            let (curr, nx) = takeTill 0 xs
                currE = tokensToExpr [] [] curr
                in tokensToExpr (currE:es) l nx
        takeTill 0 (Popen:xs) = ([],xs)
        takeTill n (Popen:xs) = let (r,m) = takeTill (n-1) xs in (Popen:r, m)
        takeTill n (Pclose:xs) = let (r, m) = takeTill (n+1) xs in (Pclose:r, m)
        takeTill n (x:xs) = let (r,m) = takeTill n xs in (x:r, m)
    return $ tokensToExpr [] [] (reverse tokens)
