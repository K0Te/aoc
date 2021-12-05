{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 where

import           Control.Lens  hiding ((^.), (^..))
import           Data.Bits     (Bits (setBit, shift, zeroBits))
import           Data.Foldable (foldl1)
import qualified Data.Map      as M
import           File          (readLinesT)
import           Import        hiding (Down, to, (%~))
import           Prelude       (print)
import qualified RIO.Text      as T

-- Issues:
-- - BitCount is hardcoded - 11
-- - Manual recursion for `findNum`
-- - Stat is reused for {0,1} and for real statistics - concise but confusing

main :: IO ()
main = do
    lines_ <- readLinesT "data/4.txt"
    print lines_