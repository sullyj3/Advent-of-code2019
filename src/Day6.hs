{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DeriveFoldable #-}
module Day6 where

import Flow
import Data.Functor ((<&>))
import Debug.Trace (trace)

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Append.Strict as AM
import Data.Map.Append.Strict (AppendMap(..))
import qualified Data.Set as Set
import Data.Set (Set)

import Data.Monoid (Sum(..), getSum)
import Data.Foldable (foldMap, traverse_, find)

import Data.List.Split (splitOn)

data Orbit = Orbit String String
  deriving Show

parseOrbit :: String -> Orbit
parseOrbit s = let 
    [s1,s2] = splitOn ")" s
  in
    Orbit s1 s2

parseOrbits :: String -> [Orbit]
parseOrbits = lines .> map parseOrbit

getOrbits :: IO [Orbit]
getOrbits = readFile "input/6.txt" <&> parseOrbits

example :: IO [Orbit]
example = readFile "input/6example.txt" <&> parseOrbits

doPart1 = getOrbits <&> part1
doPart2 = getOrbits <&> part2

part1 :: [Orbit] -> Int
part1 = orbitMap
     .> toTree
     .> annotateDepth
     .> treeMap fst
     .> foldMap Sum
     .> getSum

part2 :: [Orbit] -> Int
part2 orbits = let

    t = orbits |> orbitMap .> toTree
    Just commonOrbit = lca "YOU" "SAN" t
    commonOrbitDepths = annotateDepth commonOrbit
    Just (youDepth, _you) = find (snd .> (=="YOU")) commonOrbitDepths
    Just (sanDepth, _san) = find (snd .> (=="SAN")) commonOrbitDepths

  in youDepth - 1 + sanDepth - 1

allBodies :: [Orbit] -> Set String
allBodies = foldl (\acc (Orbit s1 s2) -> [s1,s2] <> acc) mempty

orbitMap :: [Orbit] -> Map String (Set String)
orbitMap orbits = unAppendMap $ foldl addBodies mempty orbits
  where
    addBodies :: AppendMap String (Set String) -> Orbit -> AppendMap String (Set String)
    addBodies acc (Orbit s1 s2) = acc <> (AppendMap $ [(s1, [s2]), (s2, [])])

data RoseTree a = Node a [RoseTree a]
  deriving (Show, Eq, Ord, Foldable)

treeMap f (Node x ts) = Node (f x) (map (treeMap f) ts)

toTree :: Map String (Set String) -> RoseTree String
toTree orbitMap = go "COM"
  where go s = let children = orbitMap ! s
               in Node s (map go $ Set.toList children)

lca :: Eq a => a -> a -> RoseTree a -> Maybe (RoseTree a)
lca a b (Node x ts) = case find (\t -> a `elem` t && b `elem` t) ts
  of Just t -> case lca a b t of Nothing -> Just t
                                 Just u -> Just u
     Nothing -> Nothing


annotateDepth :: Ord a => RoseTree a -> RoseTree (Int, a)
annotateDepth t = go 0 t
  where go depth (Node x ts) = Node (depth, x) (go (depth+1) <$> ts)

printTree :: (Ord a, Show a) => RoseTree a -> IO ()
printTree t = go 0 t
  where go indent (Node x ts) = do
          putStrLn $ replicate (indent * 2) ' ' <> show x
          traverse_ (go $ indent + 1) ts
