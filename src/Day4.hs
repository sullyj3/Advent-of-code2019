{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

module Day4 where

import Flow
import Data.List (group)


digits :: Int -> [Int]
digits n = reverse $ revDigits n
  where
    revDigits n
      | n < 10 = [n]
      | otherwise = (n `mod` 10) : revDigits (n `div` 10) 

fromDigits = foldl1 (\x y -> 10*x+y)


monotonicize :: Int -> Int
monotonicize = fromDigits . monotonicizeDigits . digits
  where
    monotonicizeDigits :: [Int] -> [Int]
    monotonicizeDigits (d:ds) = d : go d ds

    go currMax [] = []
    go currMax rest@(d:ds)
      | d < currMax = currMax <$ rest
      | otherwise   = d : go d ds


containsRepeat :: Eq a => [a] -> Bool
containsRepeat = group .> any (length .> (>=2))


containsDouble :: Eq a => [a] -> Bool
containsDouble = group .> any (length .> (==2))


count :: (a -> Bool) -> [a] -> Int
count p = length . filter p


part1 = monotonicize start
  |> iterate ((+1) .> monotonicize)
  .> takeWhile (< end)
  .> count (digits .> containsRepeat)
  where
    (start, end) = (235741,706948)

part2 = monotonicize start
  |> iterate ((+1) .> monotonicize)
  .> takeWhile (< end)
  .> count (digits .> containsDouble)
  where
    (start, end) = (235741,706948)

