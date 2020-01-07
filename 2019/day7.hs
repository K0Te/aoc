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
import Data.List (permutations, maximum)

x = [3,8,1001,8,10,8,105,1,0,0,21,38,63,80,105,118,199,280,361,442,99999,3,9,102,5,9,9,1001,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,4,9,9,102,2,9,9,101,2,9,9,4,9,99,3,9,1001,9,5,9,102,4,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,5,9,101,3,9,9,102,5,9,9,101,3,9,9,4,9,99,3,9,1002,9,2,9,1001,9,4,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99]
-- x = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0] -- 43210 (from phase setting sequence 4,3,2,1,0)
-- x = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0] -- 54321 (from phase setting sequence 0,1,2,3,4)
-- x = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0] -- 65210 (from phase setting sequence 1,0,4,3,2)
-- P2
-- x = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
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

data ComputerIO m k
  = ReadIO (Int -> m k)
  | PutIO Int (m k)
  deriving (Functor, Generic1)

instance HFunctor ComputerIO
instance Effect   ComputerIO

readIO :: Has ComputerIO sig m => m Int
readIO = send (ReadIO pure) --  why pure ?

putIO :: Has ComputerIO sig m => Int -> m ()
putIO s = send (PutIO s (pure ()))

newtype ComputerIOC m a = ComputerIOC { runComputerIO :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (ComputerIO :+: sig) (ComputerIOC m) where
  alg (L (ReadIO    k)) = liftIO readLn      >>= k
  alg (L (PutIO s k)) = liftIO (print s) >>  k
  alg (R other)       = ComputerIOC (alg (handleCoercible other))

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


-- runOpCode :: State -> IO (Maybe State)
runOpCode state@(State current memory) = do
  let cmd = decodeOpCode state
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
      res <- Main.readIO
      let new_m = memory // [(x, res)]
      return $ Just (State (current + 2) new_m)
    Put a1 -> do
      putIO $ addToVal memory a1
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

runTillFail :: State -> IO ()
runTillFail state = do
  mbres <- runComputerIO (runOpCode state)
  if isNothing mbres
    then return ()
    else runTillFail (fromJust mbres)

runComputerPure :: ([Int], [Int]) -> ComputerPure m a -> m (([Int], [Int]), a)
runComputerPure s = runState s . runComputerPureC

newtype ComputerPure m a = ComputerPure { runComputerPureC :: StateC ([Int], [Int]) m a }
  deriving (Applicative, Functor, Monad)

instance (Algebra sig m, Effect sig) => Algebra (ComputerIO :+: sig) (ComputerPure m) where
  alg (L (ReadIO    k)) = do
    (inp, out:: [Int]) <- ComputerPure get
    case inp of
      []  -> k 999999 -- howto fail here ?
      h:t -> ComputerPure (put (t,out)) *> k h
  alg (L (PutIO s k)) = do
    (inp:: [Int], out:: [Int]) <- ComputerPure get
    ComputerPure (put (inp, s:out)) *> k
  alg (R other)       = ComputerPure (alg (R (handleCoercible other)))

runTillFailPure :: State -> ([Int], [Int]) -> [Int]
runTillFailPure state state_other = do
  (state_other_new, mbres) <- runComputerPure state_other (runOpCode state)
  if isNothing mbres
    then (snd state_other_new)
    else runTillFailPure (fromJust mbres) state_other_new

runTillOutputPure :: State -> ([Int], [Int]) -> Maybe (State, Int, ([Int], [Int]))
runTillOutputPure state state_other = do
  (state_other_new, mbres) <- runComputerPure state_other (runOpCode state)
  if isNothing mbres
    then Nothing
    else if Prelude.null (snd state_other_new) then runTillOutputPure (fromJust mbres) state_other_new
         else Just (fromJust mbres, (snd state_other_new) !! 0, (fst state_other_new, []))

main = do
  let initial = State 0 xv
  -- print $ runTillFailPure initial ([4,0], [])
  print $ findOptimal


-- findOptimal :: Int
-- findOptimal =
--   let r = do
--         [a,b,c,d,e] <- permutations [5..9]
--         let initial = State 0 xv
--         let [a_out] = runTillFailPure initial ([a,0], [])
--         let [b_out] = runTillFailPure initial ([b,a_out], [])
--         let [c_out] = runTillFailPure initial ([c,b_out], [])
--         let [d_out] = runTillFailPure initial ([d,c_out], [])
--         let [e_out] = runTillFailPure initial ([e,d_out], [])
--         return e_out
--   in Data.List.maximum r


findOptimal =
  let r = do
        [a,b,c,d,e] <- permutations [5..9]
        let initial = State 0 xv
        return $ runCycle 0 [a] [b] [c] [d] [e] initial initial initial initial initial
  -- in r
  in Data.List.maximum r

runCycle :: Int -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> State -> State -> State -> State -> State -> Int
runCycle ai a b c d e sa sb sc sd se =
  let r = do
      (san, a_out, _) <- runTillOutputPure sa (a ++ [ai], [])
      (sbn, b_out, _) <- runTillOutputPure sb (b ++ [a_out], [])
      (scn, c_out, _) <- runTillOutputPure sc (c ++ [b_out], [])
      (sdn, d_out, _) <- runTillOutputPure sd (d ++ [c_out], [])
      (sen, e_out, _) <- runTillOutputPure se (e ++ [d_out], [])
      return (e_out, san, sbn, scn, sdn, sen)
  in if isNothing r then ai else let (e_out, san, sbn, scn, sdn, sen) = fromJust r in runCycle e_out [] [] [] [] [] san sbn scn sdn sen

