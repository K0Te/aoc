{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Day9 where

import Import
import RIO.Partial (fromJust, read)
import qualified RIO.Text as T
import qualified RIO.Set as S
import qualified Data.Text as TT
import qualified Data.Attoparsec.Text as PT
import RIO.List.Partial ((!!), head)
import Control.Monad.ST
import Data.STRef
import Conduit as C
import Data.List (subsequences)
import RIO.List (inits)

main :: HasLogFunc env => RIO env ()
main = do
    fileData <- readFileUtf8 "data/9.txt"
    let input = read <$> (TT.unpack <$> TT.lines fileData) :: [Int]
    logInfo (displayShow input)
    let res = runConduitPure (C.yieldMany input .| solution  .| sinkList)
    logInfo (displayShow res)
    let i2 = reverse [17,14,2,35,39,31,5,25,1,29,40,48,9,37,21,7,41,8,15,28,47,13,16,50,45,30,11,3,33,78,4,69,5,58,17,19,10,12,9,25,18,7,14,8,55,21,15,22,13,16,70,20,11,23,29,24,26,27,69,28,17,19,30,43,48,25,33,31,32,41,39,35,40,34,66,36,37,49,52,42,44,45,46,47,85,51,56,72,57,73,67,63,69,104,76,70,84,71,107,82,78,79,90,86,149,110,200,93,98,108,113,119,120,124,130,132,161,139,210,141,148,170,278,186,172,157,165,184,179,191,201,206,212,340,221,232,267,289,254,401,271,280,287,298,377,322,327,329,363,336,468,344,370,380,392,407,556,433,453,488,521,534,525,541,760,551,567,627,958,663,649,838,743,904,680,714,1055,984,772,1231,1248,1756,886,941,1009,1046,1059,1118,1294,1178,1265,1194,1276,1377,2383,1827,1689,1394,1726,1452,1486,1658,1925,2162,1895,2164,2000,1932,2055,2068,2105,2177,2296,3194,3993,2459,2470,2653,2880,3381,3178,3110,2846,4836,4105,3144,3553,3925,3950,3827,3932,3987,4000,4123,4564,4282,4473,5176,4929,5112,9948,5123,5499,5956,5990,7060,6254,6673,7426,6697,11430,7777,7759,7814,7827,7932,7987,8123,8405,8755,10536,9402,10041,10919,13977,10622,12183,11455,17386,29569,12927,12951,14450,15900,14456,15573,15591,24655,15641,15759,15919,16110,24993,17160,25078,25134,23858,29618,21541,41024,27096,30350,24382,25878,27377,31491,27401,28906,30029,30047,31164,31232,31400,45016,46778,41018,46139,56228,44256,45399,45923,47419,86040,48637,50260,51478,51759,53288,54784,57406,57448,76180,58935,60076,95276,62396,137518,72418,85274,94197,86417,104332,92818,96056,91322,93342,155738,98897,100115,213144,103237,105047,164408,112190,114854,116383,169522,145350,157692,185519,134814,228156,158835,187378,206387,214969,184140,197865,184664,193457,192239,262739,205162,203352,281712,296854,217237,227044,228573,249668,251197,280164,415102,371518,318954,413237,342975,343499,390730,376379,500206,368804,376903,397401,443125,420812,408514,507208,753282,695857,570543,444281,626571,592643,500865,693401,599118,661929,814643,662453,712303,852795,788131,797715,745183,745707,777318,887406,1174719,1308218,909379,1101915,1458010,1773837,945146,1407636,1036924,1099983,1093508,1162794,1782631,1344301,1920426,1374756,1630113,1457486,1543422,2345142,1522501,1690853,1523025,1987389,1796785,2494410,2936631,1854525,1982070,2038654,3545378,2199718,2130432,2136907,2193491,5236231,3144864,2719057,2801787,3213354,4016911,2979987,4409910,3045526,3213878,3653457,3510414,3319810,8889688,4232145,3836595,3893179]
    logInfo (displayShow $ solve2 i2)


-- solution :: Monad m => ConduitT Int Int m ()
solution = do
    initial <- (fmap . fmap) fromJust $ forM [1..25] (\_ -> await)
    let
        -- go :: [Int] -> ConduitT Int Int m Int
        go current = do
            mNew <- await
            maybe (return ()) (\new -> do
                case [(x,y) | x<-current, y<-current, x /= y, x+y==new] of
                    [] -> yield new
                    _  -> return ()
                go $ (drop 1 current) ++ [new]
                )  mNew 
    go initial
    return ()

secretKey :: Int
secretKey = 10884537

solve2 :: [Int] -> [[Int]]
solve2 input = let variants = nonEmptySubsequences input in
    filter (\x -> sum x == secretKey) variants


nonEmptySubsequences         :: [Int] -> [[Int]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : if shouldRecurse then withX ++ (nonEmptySubsequences xs) else []
  where f ys r = if isXUseful then ys : (x : ys) : r else ys : r
        withX = (x:) <$> inits xs
        -- isXUseful = x < secretKey
        -- shouldRecurse = sum xs > secretKey -- WRONG (!) -> without shouldRecurse some values are no longer suffixes (!)
        isXUseful = True
        shouldRecurse = True
