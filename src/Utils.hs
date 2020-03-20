module Utils
( digits
) where

digits :: Int -> [Int]
digits n = reverse $ revDigits n
  where
    revDigits n
      | n < 10 = [n]
      | otherwise = (n `mod` 10) : revDigits (n `div` 10) 
