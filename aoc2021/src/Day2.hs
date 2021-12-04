{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import           Control.Lens         hiding ((^.), (^..))
import qualified Data.Attoparsec.Text as PT
import           File                 (readLinesT)
import           Import               hiding (Down, (%~))
import           Prelude              (print)

main :: IO ()
main = do
    lines_ <- readLinesT "data/2.txt"
    print $ parseList lines_
    print $ finalPosition $ parseList lines_
    print $ solve1 lines_
    print $ solve2 lines_

data Move = Forward Int | Down Int | Up Int
    deriving (Eq, Show)

solve1 :: [Text] -> Int
solve1 x = productOf each $ finalPosition $ parseList x

parseMove :: Text -> Either String Move
parseMove = PT.parseOnly (parseForward <|> parseDown <|> parseUp)
    where
        parseForward = do
             _ <- PT.string "forward "
             Forward <$> PT.decimal
        parseDown = do
             _ <- PT.string "down "
             Down <$> PT.decimal
        parseUp = do
             _ <- PT.string "up "
             Up <$> PT.decimal

parseList :: [Text] -> [Move]
parseList t = t ^.. folded . Control.Lens.to parseMove . _Right

performMove :: (Int, Int) -> Move -> (Int, Int)
performMove pos (Forward v) = pos & _1 %~ (+v)
performMove pos (Up v)      = pos & _2 %~ (+ (-v))
performMove pos (Down v)    = pos & _2 %~ (+v)

finalPosition :: [Move] -> (Int, Int)
finalPosition = foldl' performMove (0, 0)


performMove2 :: (Int, Int, Int) -> Move -> (Int, Int, Int)
performMove2 pos (Forward v) = pos &
    _1 %~ (+ v )
    & _3 %~ (+ v * (pos ^. _2) )
performMove2 pos (Up v)      = pos & _2 %~ (+ (-v))
performMove2 pos (Down v)    = pos & _2 %~ (+v)

finalPosition2 :: [Move] -> (Int, Int, Int)
finalPosition2 = foldl' performMove2 (0, 0, 0)

solve2 :: [Text] -> Int
solve2 x = let res = finalPosition2 $ parseList x in res ^. _1 * res ^. _3
