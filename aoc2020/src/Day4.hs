{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day4 where

import Import
import RIO.Partial (read)
import qualified RIO.Text as T
import qualified RIO.Map as M
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT

type Passport = Map Text Text

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/4.txt"
    let input = snd . partitionEithers $ (parsePassport) <$> (TT.replace "\n" " " <$> TT.splitOn "\n\n" fileData) 
    logInfo (displayShow input)
    logInfo (displayShow $ solve2 input)

parsePassport :: Text -> Either String Passport
parsePassport s = M.fromList <$> (PT.parseOnly parser s)
    where parser = PT.many' $ do
            key <- T.pack <$> PT.manyTill PT.anyChar (PT.char ':')
            value <- PT.takeTill (==' ')
            _ <- PT.many' (PT.char ' ')
            return (key, value)

isValid :: Passport -> Maybe Bool
isValid pt = do
    let validations = [
           ("byr", validateBirth)
         , ("iyr", validateIssue)
         , ("eyr", validateExp)
         , ("hgt", validateHeight)
         , ("hcl", validateColor)
         , ("ecl", validateEColor)
         , ("pid", validatePid) ]
    results <- mapM (\(key, validation) -> join $ validation <$> (M.lookup key pt)) validations
    return $ all id results

maybeParse p s = case PT.parseOnly p s of
    Left _ -> Nothing
    Right x -> Just x

validateBirth :: Text -> Maybe Bool
validateBirth input = do
    year <- maybeParse PT.decimal input
    return $ year >= 1920 && year <= 2002

validateIssue :: Text -> Maybe Bool
validateIssue input = do
    year <- maybeParse PT.decimal input
    return $ year >= 2010 && year <= 2020

validateExp :: Text -> Maybe Bool
validateExp input = do
    year <- maybeParse PT.decimal input
    return $ year >= 2020 && year <= 2030

validateHeight :: Text -> Maybe Bool
validateHeight input = do
    value <- maybeParse PT.decimal input
    if T.isSuffixOf "cm" input then
        return $ value >= 150 && value <= 193
    else
        return $ value >= 59 && value <= 76

validateColor :: Text -> Maybe Bool
validateColor input = do
    maybeParse (PT.char '#' >> PT.count 6 (PT.choice $ PT.char <$> (['a'..'f'] ++ ['0'..'9'])) >> PT.endOfInput) input
    return True

validateEColor :: Text -> Maybe Bool
validateEColor input = do
    return $ input `elem`  (TT.splitOn " " "amb blu brn gry grn hzl oth")

validatePid :: Text -> Maybe Bool
validatePid input = do
    let is_9 = T.length input == 9
        allDigits = all (`elem` ['0'..'9']) (T.unpack input)
    return $ is_9 && allDigits

solve2 :: [Passport] -> Int
solve2 = length . filter (\x -> isValid x == Just True) 
