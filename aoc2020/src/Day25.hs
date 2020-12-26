{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
module Day25 where

import Import
import RIO.Partial (fromJust, read)
import qualified RIO.Text as T
import qualified RIO.Set as S
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial ((!!), head)
import Control.Monad.ST ()
import Data.Vector ((!?))
import RIO.Vector (imap)
import Data.Vector (fromList)
import qualified RIO.Map as M
import RIO.List.Partial (minimum)
import RIO.List.Partial (maximum)
import RIO.List (iterate, cycle)
import Prelude (repeat)
import Data.Attoparsec.Text (Parser)
import Control.Applicative
import Data.List (maximumBy, foldr1)
import Data.List (sort)
import Data.List (intersperse)
import Data.List (minimumBy)
import Data.List (findIndex)

main :: HasLogFunc env => RIO env ()
main = do
    let 
        -- (cardP, doorP) = (5764801, 17807724)
        (cardP, doorP) = (1327981, 2822615)
        seq = iterate (next 7) 1
        cardI = fromJust $ findIndex (==cardP) seq
        secret = take 1 $ drop cardI $ iterate (next doorP) 1
    logInfo (displayShow cardI)
    logInfo (displayShow secret)
    --logInfo (displayShow $ drop 100 $ take 101 $ solve2 input)

next :: Int -> Int -> Int
next s v = (v * s) `mod` 20201227
