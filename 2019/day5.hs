module Main where

import Data.Vector
import Control.Monad
import Data.Maybe

x = [3,225,1,225,6,6,1100,1,238,225,104,0,1002,114,19,224,1001,224,-646,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1101,40,62,225,1101,60,38,225,1101,30,29,225,2,195,148,224,1001,224,-40,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,1001,143,40,224,101,-125,224,224,4,224,1002,223,8,223,1001,224,3,224,1,224,223,223,101,29,139,224,1001,224,-99,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,14,34,225,102,57,39,224,101,-3420,224,224,4,224,102,8,223,223,1001,224,7,224,1,223,224,223,1101,70,40,225,1102,85,69,225,1102,94,5,225,1,36,43,224,101,-92,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,1102,94,24,224,1001,224,-2256,224,4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1102,8,13,225,1101,36,65,224,1001,224,-101,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,677,226,224,1002,223,2,223,1006,224,329,1001,223,1,223,1108,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,359,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,374,101,1,223,223,1107,226,226,224,1002,223,2,223,1005,224,389,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,404,101,1,223,223,1008,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,108,677,226,224,1002,223,2,223,1006,224,434,101,1,223,223,1108,677,226,224,102,2,223,223,1005,224,449,101,1,223,223,1008,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,108,677,677,224,102,2,223,223,1005,224,479,101,1,223,223,7,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,509,101,1,223,223,107,677,226,224,1002,223,2,223,1005,224,524,1001,223,1,223,7,677,226,224,1002,223,2,223,1005,224,539,1001,223,1,223,1007,226,677,224,1002,223,2,223,1005,224,554,1001,223,1,223,8,677,677,224,102,2,223,223,1006,224,569,101,1,223,223,7,226,677,224,102,2,223,223,1006,224,584,1001,223,1,223,1008,677,677,224,102,2,223,223,1005,224,599,101,1,223,223,1007,677,677,224,1002,223,2,223,1006,224,614,101,1,223,223,1107,677,226,224,1002,223,2,223,1006,224,629,101,1,223,223,1107,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,659,1001,223,1,223,108,226,226,224,102,2,223,223,1006,224,674,101,1,223,223,4,223,99,226]
-- x = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
-- x = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
-- x = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
xv = fromList x

data State = State Int (Vector Int) deriving (Show)
data Addr = Pos Int | Val Int deriving (Show)
data Cmd =  Add Addr Addr Addr |
            Mul Addr Addr Addr |
            Read Addr |
            Put Addr |
            Halt |
            JumpTrue Addr Addr |
            JumpFalse Addr Addr |
            Lt Addr Addr Addr |
            Eq Addr Addr Addr
            deriving (Show)

decodeOpCode :: State -> Cmd
decodeOpCode (State current memory) =
  let
    cmdn = memory ! current
    (prefix, code) = (cmdn `divMod` 100)
    (p3v, prefix2) = prefix `divMod` 100
    (p2v, p1v) = prefix2 `divMod` 10
    prefix2constructor x = if x == 1 then Val else Pos
    p1 = prefix2constructor p1v
    p2 = prefix2constructor p2v
    p3 = prefix2constructor p3v
  in case code of
    1 -> Add (p1 $ memory ! (current+1)) (p2 $ memory ! (current+2)) (p3 $ memory ! (current+3))
    2 -> Mul (p1 $ memory ! (current+1)) (p2 $ memory ! (current+2)) (p3 $ memory ! (current+3))
    3 -> Read (p1 $ memory ! (current+1))
    4 -> Put (p1 $ memory ! (current+1))
    5 -> JumpTrue (p1 $ memory ! (current+1)) (p2 $ memory ! (current+2))
    6 -> JumpFalse (p1 $ memory ! (current+1)) (p2 $ memory ! (current+2))
    7 -> Lt (p1 $ memory ! (current+1)) (p2 $ memory ! (current+2)) (p3 $ memory ! (current+3))
    8 -> Eq (p1 $ memory ! (current+1)) (p2 $ memory ! (current+2)) (p3 $ memory ! (current+3))
    99 -> Halt

addToVal  memory (Pos x)  = memory ! x
addToVal memory (Val x) = x

runOpCode :: State -> IO (Maybe State)
runOpCode state@(State current memory) = do
  let cmd = decodeOpCode state
  print (memory ! current)
  print cmd
  case cmd of
    Add a1 a2 (Pos x) -> do
      let res = (addToVal memory a1) + (addToVal memory a2)
      let new_m = memory // [(x, res)]
      return $ Just (State (current + 4) new_m)
    Mul a1 a2 (Pos x) -> do
      let res = (addToVal memory a1) * (addToVal memory a2)
      let new_m = memory // [(x, res)]
      return $ Just (State (current + 4) new_m)
    Read (Pos x) -> do
      putStrLn "Read:"
      res <- readLn
      let new_m = memory // [(x, res)]
      return $ Just (State (current + 2) new_m)
    Put a1 -> do
      putStrLn "Put:"
      print $ addToVal memory a1
      return $ Just (State (current + 2) memory)
    JumpTrue a1 a2 -> do
      if addToVal memory a1 /= 0
        then return $ Just (State (addToVal memory a2) memory)
        else return $ Just (State (current + 3) memory)
    JumpFalse a1 a2 -> do
      if addToVal memory a1 == 0
        then return $ Just (State (addToVal memory a2) memory)
        else return $ Just (State (current + 3) memory)
    Lt a1 a2 (Pos x) -> do
      let val = if addToVal memory a1 < addToVal memory a2 then 1 else 0
      let new_m = memory // [(x, val)]
      return $ Just (State (current + 4) new_m)
    Eq a1 a2 (Pos x) -> do
      let val = if addToVal memory a1 == addToVal memory a2 then 1 else 0
      let new_m = memory // [(x, val)]
      return $ Just (State (current + 4) new_m)
    Halt -> return Nothing

runTillFail state = do
  print state
  next_st <- runOpCode state
  guard $ not $ isNothing next_st
  runTillFail (fromJust next_st)


main = do
  let initial = State 0 xv
  runTillFail initial





