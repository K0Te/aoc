{-# LANGUAGE NoImplicitPrelude #-}
module Day1 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/1.txt"
    let input = (read . T.unpack) <$> (T.lines fileData) 
    logInfo (displayShow $ solve1 input)
    logInfo (displayShow $ solve2 input)

solve1 :: [Int] -> [Int]
solve1 input = [x*y | x <- input, y<-input, x+y==2020]

solve2 :: [Int] -> [Int]
solve2 input = [x*y*z | x <- input, y<-input, z<-input, x+y+z==2020]