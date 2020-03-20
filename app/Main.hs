{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Day1 hiding (main)
import qualified Day2 hiding (main)
import qualified Day3 hiding (main)
import qualified Day4 hiding (main)
import qualified Day5 hiding (main)
import qualified Day6 hiding (main)

import System.Environment

(<$$>) = (<$>) . (<$>)

main :: IO ()
main = do
  [day, part] :: [Int] <- read <$$> getArgs

  case day of
    1 -> Day1.run part
    -- 2 -> Day2.run
    -- 3 -> Day3.run
    -- 4 -> Day4.run
    -- 5 -> Day5.run
    -- 6 -> Day6.run
    -- 7 -> Day7.run

