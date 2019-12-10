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
part1 = sum . map fuel

part2 :: [Int] -> Int
part2 = sum . map part2Fuel

part2Fuel :: Int -> Int
part2Fuel mass = sum $ unfoldr fuelOfFuel mass

fuelOfFuel :: Int -> Maybe (Int, Int)
fuelOfFuel mass
  | mass <= 0 = Nothing
  | otherwise = let next = fuel mass in
    if next > 0 then Just (next, next)
                else Nothing
