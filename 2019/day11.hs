{-# LANGUAGE DeriveFunctor, DeriveGeneric, OverlappingInstances, FlexibleInstances, FlexibleContexts, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeOperators, UndecidableInstances, ScopedTypeVariables #-}

module Main where

import Data.Vector hiding ((++), singleton, union, empty)
import Control.Monad
import Data.Maybe
import Control.Algebra
import Control.Carrier.State.Strict hiding (State, Put)
import Control.Carrier.Writer.Strict
import Control.Monad.IO.Class
import GHC.Generics (Generic1)
import Prelude hiding (readIO, Right, Left)
import Data.List (permutations, maximum, take, repeat)
import Data.Set (singleton, union, difference, Set, member, empty, size)

x = [3,8,1005,8,309,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,29,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,51,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,72,1,1104,8,10,2,1105,15,10,2,1106,0,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,101,0,8,107,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,128,2,6,8,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,155,1006,0,96,2,108,10,10,1,101,4,10,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,188,2,1,5,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,214,2,6,18,10,1006,0,78,1,105,1,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,102,1,8,247,2,103,8,10,2,1002,10,10,2,106,17,10,1,1006,15,10,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,285,1,1101,18,10,101,1,9,9,1007,9,992,10,1005,10,15,99,109,631,104,0,104,1,21102,387507921664,1,1,21102,1,326,0,1106,0,430,21102,932826591260,1,1,21102,337,1,0,1106,0,430,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,206400850983,0,1,21101,0,384,0,1105,1,430,21102,3224464603,1,1,21102,395,1,0,1106,0,430,3,10,104,0,104,0,3,10,104,0,104,0,21102,838433657700,1,1,21102,418,1,0,1106,0,430,21101,825012007272,0,1,21101,429,0,0,1106,0,430,99,109,2,21202,-1,1,1,21101,40,0,2,21101,461,0,3,21102,1,451,0,1105,1,494,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,456,457,472,4,0,1001,456,1,456,108,4,456,10,1006,10,488,1102,1,0,456,109,-2,2106,0,0,0,109,4,1202,-1,1,493,1207,-3,0,10,1006,10,511,21101,0,0,-3,21202,-3,1,1,21201,-2,0,2,21102,1,1,3,21102,1,530,0,1106,0,535,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,558,2207,-4,-2,10,1006,10,558,22101,0,-4,-4,1106,0,626,22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,0,577,0,1106,0,535,22102,1,1,-4,21101,1,0,-1,2207,-4,-2,10,1006,10,596,21102,0,1,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,618,21201,-1,0,1,21102,618,1,0,105,1,493,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]
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

data Direction = Left | Right | Up | Down deriving (Eq, Show)
data Location = Location Int Int deriving (Eq, Show, Ord)
data RobotState = RobotState Direction Location Bool deriving (Eq, Show)
data MoveCmd = Lft | Rht deriving (Eq, Show)

moveRobot :: RobotState -> MoveCmd -> RobotState
moveRobot (RobotState dir (Location x y) z) mcmd = RobotState newDir newLoc False
  where
    move Left Lft = Down
    move Right Lft = Up
    move Up Lft = Left
    move Down Lft = Right
    move Left Rht = Up
    move Right Rht = Down
    move Up Rht = Right
    move Down Rht = Left
    newDir = move dir mcmd
    newLoc = case newDir of
      Up -> Location x (y+1)
      Down -> Location x (y-1)
      Right -> Location (x+1) y
      Left -> Location (x-1) y


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
  alg (L (ReadIO    k)) = liftIO (print "?") >>   liftIO readLn      >>= k
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

runComputerPure :: (RobotState, Set Location, Set Location) -> ComputerPure m a -> m ((RobotState, Set Location, Set Location), a)
runComputerPure s = runState s . runComputerPureC

newtype ComputerPure m a = ComputerPure { runComputerPureC :: StateC (RobotState, Set Location, Set Location) m a }
  deriving (Applicative, Functor, Monad)

instance (Algebra sig m, Effect sig) => Algebra (ComputerIO :+: sig) (ComputerPure m) where
  alg (L (ReadIO k)) = do
    ((RobotState dir loc z), visitedLocs:: Set Location, whiteLocs) <- ComputerPure get
    if loc `member` whiteLocs then k 1 else k 0
  alg (L (PutIO s k)) = do
    (rst@(RobotState dir loc moving), visitedLocs, whiteLocs) <- ComputerPure get
    if moving
      then let mcmd = (if s == 1 then Rht else Lft) in ComputerPure (put (moveRobot rst mcmd, visitedLocs, whiteLocs) ) *> k
      else if s == 1 then ComputerPure (put (setMoving rst, visitedLocs `union` (singleton loc), whiteLocs `union` (singleton loc)) ) *> k
                     else ComputerPure ( put (setMoving rst, visitedLocs `union` (singleton loc), whiteLocs `difference` (singleton loc)) ) *> k
  alg (R other)       = ComputerPure (alg (R (handleCoercible other)))

setMoving (RobotState x y z) = RobotState x y True

runTillFailPure :: State -> (RobotState, Set Location, Set Location) -> IO (Set Location)
runTillFailPure state state_other = do
  (state_other_new@(rst, vst, white), mbres) <- runComputerPure state_other (runOpCode state)
  if isNothing mbres
    then return white
    else runTillFailPure (fromJust mbres) state_other_new

main = do
  let initial = State 0 xv 0
  res <- runTillFailPure initial (RobotState Up (Location 0 0) False, empty, singleton (Location 0 0))
  printRes res

printRes locs = do
  Control.Monad.forM_ [0..5] (\line -> do
    let res = [if Location x (-line) `member` locs then '#' else '.' | x<-[0..40]]
    print res
    )















