{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExplicitForAll #-}
--{-# LANGUAGE BlockArguments #-}
--
module Day2 where

import Control.Monad.ST

import Data.STRef
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed

import Data.Array.IArray ((!))

import Data.List (find)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

main = do
  input <- readFile "input.txt"

  let intcodes :: [Int]
      intcodes = map read . splitOn "," $ input

      initialMem :: UArray Int Int
      initialMem = listArray (0, length intcodes - 1) intcodes

  putStrLn "part 1"
  print $ part1 initialMem

  putStrLn "part 2"
  print $ part2 initialMem

part1 :: UArray Int Int -> Int
part1 initialMem = finalMem ! 0 where
  finalMem :: UArray Int Int
  finalMem = execute 12 2 initialMem

part2 :: UArray Int Int -> Int
part2 initialMem = 100 * noun + verb where
  Just (noun, verb) = find (\(n,v) -> execute n v initialMem ! 0 == 19690720)
                      [ (n,v) | n <- [0..99], v <- [0..99] ]

execute :: Int -> Int -> UArray Int Int -> UArray Int Int
execute noun verb initialMem = runSTUArray $ do
  memory <- thaw initialMem
  writeArray memory 1 noun
  writeArray memory 2 verb

  -- run program
  instructPtrRef <- newSTRef 0

  let loop = do
        instructPtr <- readSTRef instructPtrRef
        op <- readArray memory instructPtr
        case op of
          1 -> runOp (+) instructPtrRef memory >> loop
          2 -> runOp (*) instructPtrRef memory >> loop
          99 -> return ()
          n -> return () -- invalid opCode

  loop
  return memory

runOp :: (Int -> Int -> Int)
      -> STRef s Int 
      -> STUArray s Int Int 
      -> ST s ()
runOp op instructPtrRef arr = do
  instructPtr <- readSTRef instructPtrRef

  posA <- readArray arr (instructPtr + 1)
  posB <- readArray arr (instructPtr + 2)

  a <- readArray arr posA
  b <- readArray arr posB

  writeLoc <- readArray arr (instructPtr + 3)
  writeArray arr writeLoc (a `op` b)
  modifySTRef instructPtrRef (+4)
