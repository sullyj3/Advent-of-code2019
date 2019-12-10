import Flow
import Data.List (unfoldr)

main = do
  input <- readFile "input.txt"

  let masses :: [Int]
      masses = map read . lines $ input 

  putStrLn "part 1"
  print $ part1 masses

  putStrLn ""

  putStrLn "part 2"
  print $ part2 masses

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

part1 :: [Int] -> Int
part1 moduleMasses = sum . map fuel $ moduleMasses

part2 :: [Int] -> Int
part2 moduleMasses = sum . map part2Fuel $ moduleMasses

part2Fuel :: Int -> Int
part2Fuel mass = sum
               . takeWhile (>0)
               . tail
               . iterate fuelOfFuel
               $ mass

fuelOfFuel :: Int -> Int
fuelOfFuel mass = max 0 (fuel mass)
