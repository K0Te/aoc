module Main where

import Data.List
import Data.List.Split

nums = [124075..580769]
nums_strs = show <$> nums

isValid x = twoAdjcent x && isMonthonic x
isValid2 x = twoAdjcent2 x && isMonthonic x


twoAdjcent x = foldl1 (||) $ zipWith (==) x (tail x)
isMonthonic x = foldl1 (&&) $ zipWith (<=) x (tail x)
twoAdjcent2 x = not (null has_valid_subs) && foldl1 (||) has_valid_subs
  where subs = subsequences x
        same_char_subs = filter (\s -> (length s) > 1  && (foldl1 (&&) $ zipWith (==) s (tail s))) subs
        has_valid_subs = (\s -> length s == 2 && not ( any (\x -> isPrefixOf s x && x/=s) same_char_subs)) <$> same_char_subs

solutions = filter isValid nums_strs
solutions2 = filter isValid2 nums_strs
main = print (length solutions2)