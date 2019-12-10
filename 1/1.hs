
main = do
  input <- readFile "input.txt"

  let masses :: [Int]
      masses = map read . lines $ input 

  print $ day1 masses

fuel :: Int -> Int
fuel mass = mass `div` 3 - 2

day1 :: [Int] -> Int
day1 = sum . map fuel
