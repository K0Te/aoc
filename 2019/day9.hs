{-# LANGUAGE DeriveFunctor, DeriveGeneric, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}

module Main where

import Data.Vector hiding ((++))
import Control.Monad
import Data.Maybe
import Control.Algebra
import Control.Carrier.State.Strict hiding (State, Put)
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import Prelude hiding (readIO)
import Data.List (permutations, maximum, take, repeat)

x = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1102,3,1,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,902,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1101,309,0,1024,1101,0,24,1002,1102,388,1,1029,1102,1,21,1019,1101,0,33,1015,1102,1,304,1025,1101,344,0,1027,1101,25,0,1003,1102,1,1,1021,1101,29,0,1012,1101,0,23,1005,1102,1,32,1007,1102,38,1,1000,1101,30,0,1016,1102,1,347,1026,1101,0,26,1010,1101,0,39,1004,1102,1,36,1011,1101,0,393,1028,1101,0,37,1013,1101,0,35,1008,1101,34,0,1001,1101,0,495,1022,1102,1,28,1018,1101,0,0,1020,1102,1,22,1006,1101,488,0,1023,1102,31,1,1009,1102,1,20,1017,1101,0,27,1014,109,10,21102,40,1,4,1008,1014,37,63,1005,63,205,1001,64,1,64,1106,0,207,4,187,1002,64,2,64,109,-18,1207,8,37,63,1005,63,227,1001,64,1,64,1106,0,229,4,213,1002,64,2,64,109,17,1207,-7,25,63,1005,63,247,4,235,1106,0,251,1001,64,1,64,1002,64,2,64,109,-8,1202,6,1,63,1008,63,29,63,1005,63,275,1001,64,1,64,1106,0,277,4,257,1002,64,2,64,109,25,1205,-6,293,1001,64,1,64,1105,1,295,4,283,1002,64,2,64,109,-4,2105,1,2,4,301,1106,0,313,1001,64,1,64,1002,64,2,64,109,-9,1208,-4,31,63,1005,63,335,4,319,1001,64,1,64,1105,1,335,1002,64,2,64,109,16,2106,0,-2,1106,0,353,4,341,1001,64,1,64,1002,64,2,64,109,-13,2102,1,-8,63,1008,63,38,63,1005,63,373,1105,1,379,4,359,1001,64,1,64,1002,64,2,64,109,9,2106,0,3,4,385,1105,1,397,1001,64,1,64,1002,64,2,64,109,-11,21107,41,42,0,1005,1014,415,4,403,1106,0,419,1001,64,1,64,1002,64,2,64,109,14,1206,-7,431,1106,0,437,4,425,1001,64,1,64,1002,64,2,64,109,-23,2107,37,-5,63,1005,63,455,4,443,1105,1,459,1001,64,1,64,1002,64,2,64,109,10,21107,42,41,-2,1005,1013,475,1105,1,481,4,465,1001,64,1,64,1002,64,2,64,2105,1,8,1001,64,1,64,1106,0,497,4,485,1002,64,2,64,109,-6,21108,43,41,8,1005,1017,517,1001,64,1,64,1106,0,519,4,503,1002,64,2,64,109,5,2101,0,-9,63,1008,63,23,63,1005,63,541,4,525,1106,0,545,1001,64,1,64,1002,64,2,64,109,-13,1201,5,0,63,1008,63,20,63,1005,63,565,1105,1,571,4,551,1001,64,1,64,1002,64,2,64,109,16,1205,4,589,4,577,1001,64,1,64,1106,0,589,1002,64,2,64,109,-16,1202,4,1,63,1008,63,23,63,1005,63,615,4,595,1001,64,1,64,1106,0,615,1002,64,2,64,109,1,2101,0,6,63,1008,63,33,63,1005,63,639,1001,64,1,64,1105,1,641,4,621,1002,64,2,64,109,8,21101,44,0,8,1008,1018,44,63,1005,63,667,4,647,1001,64,1,64,1105,1,667,1002,64,2,64,109,-7,1201,1,0,63,1008,63,39,63,1005,63,689,4,673,1106,0,693,1001,64,1,64,1002,64,2,64,109,7,2102,1,-8,63,1008,63,24,63,1005,63,715,4,699,1105,1,719,1001,64,1,64,1002,64,2,64,109,5,2108,34,-7,63,1005,63,739,1001,64,1,64,1105,1,741,4,725,1002,64,2,64,109,-22,2108,25,10,63,1005,63,763,4,747,1001,64,1,64,1106,0,763,1002,64,2,64,109,31,1206,-4,781,4,769,1001,64,1,64,1105,1,781,1002,64,2,64,109,-10,21101,45,0,5,1008,1019,47,63,1005,63,805,1001,64,1,64,1105,1,807,4,787,1002,64,2,64,109,2,21108,46,46,-3,1005,1013,825,4,813,1106,0,829,1001,64,1,64,1002,64,2,64,109,-22,2107,40,10,63,1005,63,845,1105,1,851,4,835,1001,64,1,64,1002,64,2,64,109,17,1208,-7,36,63,1005,63,871,1001,64,1,64,1105,1,873,4,857,1002,64,2,64,109,16,21102,47,1,-9,1008,1018,47,63,1005,63,899,4,879,1001,64,1,64,1106,0,899,4,64,99,21102,1,27,1,21101,0,913,0,1105,1,920,21201,1,39657,1,204,1,99,109,3,1207,-2,3,63,1005,63,962,21201,-2,-1,1,21102,1,940,0,1105,1,920,21201,1,0,-1,21201,-2,-3,1,21101,955,0,0,1105,1,920,22201,1,-1,-2,1106,0,966,21202,-2,1,-2,109,-3,2105,1,0]
-- x = [1102,34915192,34915192,7,4,7,99,0]
-- x = [104,1125899906842624,99]
xv = fromList (x ++  (Data.List.take 10000 (repeat 0)))

data State = State Int (Vector Integer) Integer deriving (Show)
data Addr = Pos Integer | Val Integer | Rel Integer deriving (Show)
data Cmd =  Add Addr Addr Addr |
            Mul Addr Addr Addr |
            Read Addr |
            Put Addr |
            Halt |
            JumpTrue Addr Addr |
            JumpFalse Addr Addr |
            Lt Addr Addr Addr |
            Eq Addr Addr Addr |
            AdjOffset Addr
            deriving (Show)

data ComputerIO m k
  = ReadIO (Integer -> m k)
  | PutIO Integer (m k)
  deriving (Functor, Generic1)

instance HFunctor ComputerIO
instance Effect   ComputerIO

readIO :: Has ComputerIO sig m => m Integer
readIO = send (ReadIO pure) --  why pure ?

putIO :: Has ComputerIO sig m => Integer -> m ()
putIO s = send (PutIO s (pure ()))

newtype ComputerIOC m a = ComputerIOC { runComputerIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (ComputerIO :+: sig) (ComputerIOC m) where
  alg (L (ReadIO    k)) = liftIO readLn      >>= k
  alg (L (PutIO s k)) = liftIO (print s) >>  k
  alg (R other)       = ComputerIOC (alg (handleCoercible other))

decodeOpCode :: State -> Cmd
decodeOpCode (State current memory offset) =
  let
    cmdn = memory ! current
    (prefix, code) = (cmdn `divMod` 100)
    (p3v, prefix2) = prefix `divMod` 100
    (p2v, p1v) = prefix2 `divMod` 10
    prefix2constructor 0 = Pos
    prefix2constructor 1 = Val
    prefix2constructor 2 = Rel
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
    9 -> AdjOffset (p1 $ memory ! (current+1))
    99 -> Halt

addrToVal memory offset (Pos x)  = memory ! (fromIntegral x )
addrToVal memory offset (Val x) = x
addrToVal memory offset (Rel x) = memory ! (fromIntegral $ x+offset)

addrToDst memory offset (Pos x)  = x
addrToDst memory offset (Rel x) = x+offset

-- runOpCode :: State -> IO (Maybe State)
runOpCode state@(State current memory offset) = do
  let cmd = decodeOpCode state
  let addToVal = addrToVal memory offset
  let addToDst = addrToDst memory offset
  case cmd of
    Add a1 a2 a3 -> do
      let res = (addToVal  a1) + (addToVal  a2)
      let dst = addToDst a3
      let new_m = memory // [(fromIntegral dst, res)]
      return $ Just (State (current + 4) new_m offset)
    Mul a1 a2 a3 -> do
      let res = (addToVal  a1) * (addToVal  a2)
      let dst = addToDst a3
      let new_m = memory // [(fromIntegral dst, res)]
      return $ Just (State (current + 4) new_m offset)
    Read a1 -> do
      res <- Main.readIO
      let dst = addToDst a1
      let new_m = memory // [(fromIntegral dst, res)]
      return $ Just (State (current + 2) new_m offset)
    Put a1 -> do
      putIO $ addToVal  a1
      return $ Just (State (current + 2) memory offset)
    JumpTrue a1 a2 -> do
      if addToVal  a1 /= 0
        then return $ Just (State (fromIntegral $ addToVal  a2) memory offset)
        else return $ Just (State (current + 3) memory offset)
    JumpFalse a1 a2 -> do
      if addToVal  a1 == 0
        then return $ Just (State (fromIntegral $ addToVal  a2) memory offset)
        else return $ Just (State (current + 3) memory offset)
    Lt a1 a2 a3 -> do
      let val = if addToVal  a1 < addToVal  a2 then 1 else 0
      let dst = addToDst a3
      let new_m = memory // [(fromIntegral dst, val)]
      return $ Just (State (current + 4) new_m offset)
    Eq a1 a2 a3 -> do
      let val = if addToVal  a1 == addToVal  a2 then 1 else 0
      let dst = addToDst a3
      let new_m = memory // [(fromIntegral dst, val)]
      return $ Just (State (current + 4) new_m offset)
    AdjOffset a1 -> do
      let val = addToVal  a1
      return $ Just (State (current + 2) memory (offset+val))
    Halt -> return Nothing

runTillFail :: State -> IO ()
runTillFail state = do
  mbres <- runComputerIO (runOpCode state)
  if isNothing mbres
    then return ()
    else runTillFail (fromJust mbres)

runComputerPure :: ([Integer], [Integer]) -> ComputerPure m a -> m (([Integer], [Integer]), a)
runComputerPure s = runState s . runComputerPureC

newtype ComputerPure m a = ComputerPure { runComputerPureC :: StateC ([Integer], [Integer]) m a }
  deriving (Applicative, Functor, Monad)

instance (Algebra sig m, Effect sig) => Algebra (ComputerIO :+: sig) (ComputerPure m) where
  alg (L (ReadIO    k)) = do
    (inp:: [Integer], out:: [Integer]) <- ComputerPure get
    case inp of
      []  -> k 999999 -- howto fail here ?
      h:t -> ComputerPure (put (t,out)) *> k h
  alg (L (PutIO s k)) = do
    (inp:: [Integer], out:: [Integer]) <- ComputerPure get
    ComputerPure (put (inp, s:out)) *> k
  alg (R other)       = ComputerPure (alg (R (handleCoercible other)))

runTillFailPure :: State -> ([Integer], [Integer]) -> [Integer]
runTillFailPure state state_other = do
  (state_other_new, mbres) <- runComputerPure state_other (runOpCode state)
  if isNothing mbres
    then (snd state_other_new)
    else runTillFailPure (fromJust mbres) state_other_new

main = do
  let initial = State 0 xv 0
  print $ runTillFailPure initial ([2], [])















