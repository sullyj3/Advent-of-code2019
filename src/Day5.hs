{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day5
(
) where

import Control.Monad.ST

import Data.STRef
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed

import Data.Array.IArray ((!))

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Array.Base (unsafeFreezeSTUArray)

import Data.Functor ((<&>))
import Data.Foldable

import Utils
import Flow

import Debug.Trace (trace)

data InstructArg = Position Int | Immediate Int
  deriving Show

instructArg :: Int -> Int -> InstructArg
instructArg mode arg = arg |> case mode of
  0 -> Position
  1 -> Immediate
  _ -> error "Invalid mode"

type WriteAddress = Int

data Instruction = Add InstructArg
                       InstructArg
                       WriteAddress
                 | Mul InstructArg
                       InstructArg
                       WriteAddress
                 | Input WriteAddress
                 | Output InstructArg
                 | JumpIfTrue InstructArg InstructArg
                 | JumpIfFalse InstructArg InstructArg
                 | LessThan InstructArg InstructArg WriteAddress
                 | Equals InstructArg InstructArg WriteAddress
                 | Halt
  deriving Show


runSTUArray2 :: (forall s. ST s (STUArray s i e, a)) -> (UArray i e, a)
runSTUArray2 st = runST $ do
  (marr, a) <- st
  arr <- unsafeFreezeSTUArray marr
  pure (arr, a)

execute :: UArray Int Int -> Int -> (UArray Int Int, [Int])
execute initialMem input = runSTUArray2 do
  memory :: STUArray s Int Int
         <- thaw initialMem

  instructPtrRef <- newSTRef 0

  let
      loop output = do
        instruction <- parseInstruction instructPtrRef memory
        case instruction of
          Add arg1 arg2 writeAddress -> do
            applyBinOp (+) arg1 arg2 writeAddress
            loop output
          Mul arg1 arg2 writeAddress -> do
            applyBinOp (*) arg1 arg2 writeAddress
            loop output
          Input writeAddress -> do
            writeArray memory writeAddress input
            modifySTRef instructPtrRef (+2)
            loop output
          Output arg -> do
            val <- resolveArg arg memory
            modifySTRef instructPtrRef (+2)
            -- since we cons on output on the left, we'll need to reverse it at the end
            loop (val:output)
          JumpIfTrue arg1 arg2 -> do
            val1 <- resolveArg arg1 memory
            if val1 /= 0
              then jump arg2
              else modifySTRef instructPtrRef (+3)
            loop output
          JumpIfFalse arg1 arg2 -> do
            val1 <- resolveArg arg1 memory
            if val1 == 0
              then jump arg2
              else modifySTRef instructPtrRef (+3)
            loop output
          LessThan arg1 arg2 writeAddress -> do
            applyBinOp (\a b -> fromEnum $ a < b) arg1 arg2 writeAddress
            loop output
          Equals arg1 arg2 writeAddress -> do
            applyBinOp (\a b -> fromEnum $ a == b) arg1 arg2 writeAddress
            loop output
          Halt -> pure output

      applyBinOp op arg1 arg2 writeAddress = do
        val1 <- resolveArg arg1 memory
        val2 <- resolveArg arg2 memory
        writeArray memory writeAddress (val1 `op` val2)
        modifySTRef instructPtrRef (+4)

      jump arg = do
        addr <- resolveArg arg memory
        writeSTRef instructPtrRef addr

  output <- reverse <$> loop []
  return (memory, output)


resolveArg :: InstructArg -> STUArray s Int Int -> ST s Int
resolveArg (Immediate n) _   = pure n
resolveArg (Position n)  arr = readArray arr n


parseInstruction :: STRef s Int -> STUArray s Int Int -> ST s Instruction
parseInstruction instructPtrRef arr = do

  instructPtr <- readSTRef instructPtrRef
  code :: Int <- readArray arr instructPtr

  let
    (modes, opCode) = (code `div` 100, code `mod` 100)
    [mode3,mode2,mode1] = padleft 0 3 (digits modes)
    instructArg = \case
      0 -> Position
      1 -> Immediate
      _ -> error "Invalid mode"

    makeBinOp op = do
      [arg1,arg2,arg3] :: [Int] <- traverse (readArray arr) [instructPtr+1..instructPtr+3]
      pure $ op (instructArg mode1 arg1)
                (instructArg mode2 arg2)
                arg3
    makeJump op = do
      [arg1,arg2] :: [Int] <- traverse (readArray arr) [instructPtr+1..instructPtr+2]
      pure $ op (instructArg mode1 arg1) (instructArg mode2 arg2)


  case opCode of
    1 -> makeBinOp Add
    2 -> makeBinOp Mul
    3 -> Input <$> readArray arr (instructPtr+1)
    4 -> Output . instructArg mode1 <$> readArray arr (instructPtr+1)
    5 -> makeJump JumpIfTrue
    6 -> makeJump JumpIfFalse
    7 -> makeBinOp LessThan
    8 -> makeBinOp Equals
    99 -> pure $ Halt
  where
    padleft :: a -> Int -> [a] -> [a]
    padleft x n [] = replicate n x
    padleft x n xs = replicate (n-length xs) x ++ xs

-------------
-- examples programs:
examples :: [UArray Int Int]
examples = codesToArray <$>
  [ -- Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
    [3,9,8,9,10,9,4,9,99,-1,8]

    -- Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
  , [3,9,7,9,10,9,4,9,99,-1,8]

    -- Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
  , [3,3,1108,-1,8,3,4,3,99]

    -- Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
  , [3,3,1107,-1,8,3,4,3,99]
  ]

codesToArray :: [Int] -> UArray Int Int
codesToArray intcodes = listArray (0, length intcodes - 1) intcodes

readInitialMem :: String -> IO (UArray Int Int)
readInitialMem fpath =
  readFile fpath <&> (splitOn "," .> map read .> codesToArray)


part1 = do
  initialMem <- readInitialMem "input/5.txt"
  let (finalMem, output) = execute initialMem 1
  -- putStrLn "Final memory"
  -- print finalMem
  putStrLn "output"
  print output

part2 = do
  initialMem <- readInitialMem "input/5.txt"
  let (finalMem, output) = execute initialMem 5
  --putStrLn "Final memory"
  --print finalMem
  putStrLn "output"
  print output
