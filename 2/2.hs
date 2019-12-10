import Control.Monad.ST

import Data.STRef
import Data.Array.ST
import Data.Array.MArray
import Data.Array.Unboxed

import Data.Array.IArray ((!))

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

main = do
  input <- readFile "input.txt"

  let intcodes :: [Int]
      intcodes = map read . splitOn "," $ input

  print $ part1 intcodes

part1 :: [Int] -> Int
part1 intcodes = finalMemory ! 0 where
  finalMemory :: UArray Int Int
  finalMemory = runSTUArray $

    do memory <- newListArray (0,length intcodes-1) intcodes

       -- restore 1202 program alarm state
       writeArray memory 1 12
       writeArray memory 2 2

       -- run program
       headRef <- newSTRef 0

       let loop = do head <- readSTRef headRef
                     op <- readArray memory head
                     case op of
                       1 -> runOp (+) headRef memory >> loop
                       2 -> runOp (*) headRef memory >> loop
                       99 -> return ()

       loop
       return memory

runOp :: (Int -> Int -> Int)
      -> STRef s Int 
      -> STUArray s Int Int 
      -> ST s ()
runOp op headRef arr = do
  head <- readSTRef headRef

  posA <- readArray arr (head + 1)
  posB <- readArray arr (head + 2)

  a <- readArray arr posA
  b <- readArray arr posB

  writeLoc <- readArray arr (head + 3)
  writeArray arr writeLoc (a `op` b)
  modifySTRef headRef (+4)
