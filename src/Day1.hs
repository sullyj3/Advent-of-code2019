module Day1
( run
)
where

import Flow
import Data.Functor
import Data.List (unfoldr)

run :: Int -> IO ()
run 1 = getMasses <&> part1 >>= print
run 2 = getMasses <&> part2 >>= print

getMasses :: IO [Int]
getMasses = readFile "input/1.txt" <&> lines .> map read

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

part1 :: [Int] -> Int
part1 moduleMasses = sum . map fuel $ moduleMasses

part2 :: [Int] -> Int
part2 moduleMasses = sum . map part2Fuel $ moduleMasses

part2Fuel :: Int -> Int
part2Fuel mass = sum
               . takeWhile (>0)
               . tail -- don't include the mass of the module itself
               . iterate fuelOfFuel
               $ mass

fuelOfFuel :: Int -> Int
fuelOfFuel mass = max 0 (fuel mass)
