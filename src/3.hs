{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Flow
import qualified Data.Set as S
import Data.Set (Set)
import Data.List.Split (splitOn)
import Control.Monad.State
import Control.Monad.Loops
import Data.Semigroup ((<>))
import Data.Ord (comparing)
import Data.Foldable (minimumBy)
import Debug.Trace


data Vec = Vec Int Int
  deriving (Show, Ord, Eq)


data Direction = U | D | L | R
  deriving (Read, Show, Eq, Ord)


data Segment = Seg { seg_dir :: Direction
                   , seg_len :: Int }
  deriving (Show, Eq, Ord)


type Wire = [Segment]


instance Num Vec where
  (Vec x1 y1) + (Vec x2 y2) = Vec (x1+x2) (y1+y2)
  (*) = undefined
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined


parseWire :: String -> Wire
parseWire s = map parseSegment . splitOn "," $ s
  where
    parseSegment :: String -> Segment
    parseSegment (c:ns) = Seg (read [c]) (read ns)
    parseSegment _ = error "segment cannot be empty"


wireToPoints :: Wire -> Set Vec
wireToPoints segs = flip evalState (Vec 0 0) (foldM insertSegPoints mempty segs)
  where
    insertSegPoints :: Set Vec -> Segment -> State Vec (Set Vec)
    insertSegPoints pts seg = do
      start <- get
      let newPoints = segmentToPoints start seg
      put $ addSegment start seg

      pure (pts <> newPoints)

data Fuck = Fuck
  deriving (Show, Eq, Ord)


distAlongWire :: Vec -> Wire -> Either Fuck Int
distAlongWire p wire = go 0 wire (Vec 0 0)
  where
    go :: Int -> Wire -> Vec -> Either Fuck Int
    go acc [] _        = Left Fuck
    go acc (seg:segs) start
      | pointInSegment seg = Right $ acc + manhattan start p
      | otherwise = go (acc + seg_len seg) segs (addSegment start seg)
      where
        pointInSegment :: Segment -> Bool
        pointInSegment seg = p `elem` segmentToPoints start seg


-- not really nicer
distAlongWire' :: Vec -> Wire -> Either Fuck Int
distAlongWire' p wire = evalState (go wire) (0, (Vec 0 0))
  where
    go :: Wire -> State (Int, Vec) (Either Fuck Int)
    go [] = pure $ Left Fuck
    go (seg:segs) = do
      (acc, start) <- get
      --pointInSegment :: Segment -> Bool
      let pointInSegment seg = p `elem` segmentToPoints start seg

      if pointInSegment seg then
        pure $ Right $ acc + manhattan start p
      else do
        put ((acc + seg_len seg), (addSegment start seg))
        go segs


-- also still pretty gross
distAlongWire'' :: Vec -> Wire -> Maybe Int
distAlongWire'' p wire = 
  let (seg, (dist, _)) = runState (firstM pointInSegment wire) (0, (Vec 0 0))
  in dist <$ seg
  where
    pointInSegment :: Segment -> State (Int, Vec) Bool
    pointInSegment seg = do
      (acc, start) <- get
      let inSegment = p `elem` segmentToPoints start seg
      if inSegment then do
        put (acc + manhattan start p, addSegment start seg)
        pure True
      else do
        put ((acc + seg_len seg), (addSegment start seg))
        pure False


addSegment :: Vec -> Segment -> Vec
addSegment (Vec x0 y0) (Seg dir len) = case dir of
  U -> Vec x0 (y0+len)
  D -> Vec x0 (y0-len)
  L -> Vec (x0-len) y0
  R -> Vec (x0+len) y0

segmentToPoints :: Vec -> Segment -> Set Vec
segmentToPoints (Vec x0 y0) (Seg dir len) = S.fromList $ case dir of
    U ->      (Vec x0) <$> (range y0 (y0+len))
    D ->      (Vec x0) <$> (range y0 (y0-len))
    L -> (flip Vec y0) <$> (range x0 (x0-len))
    R -> (flip Vec y0) <$> (range x0 (x0+len))

range :: Int -> Int -> [Int]
range a b | a <= b = [a..b]
          | otherwise = [a,a-1..b]


wireIntersections :: Wire -> Wire -> Set Vec
wireIntersections w1 w2 = S.delete (Vec 0 0) $
  wireToPoints w1 `S.intersection` wireToPoints w2


part1 :: Wire -> Wire -> Int
part1 w1 w2 = manhattan closestIntersection $ Vec 0 0
  where
    closestIntersection = minimumBy (comparing $ manhattan $ Vec 0 0) $
      wireIntersections w1 w2


part2 :: Wire -> Wire -> Either Fuck Int
part2 w1 w2 = combined leastCombined
  where
    leastCombined = minimumBy (comparing combined) $ wireIntersections w1 w2
    combined :: Vec -> Either Fuck Int
    combined p = (+) <$> distAlongWire p w1 <*> distAlongWire p w2

manhattan :: Vec -> Vec -> Int
manhattan (Vec x1 y1) (Vec x2 y2) = abs (x1-x2) + abs (y1-y2)


main = do
  input <- readFile "input.txt"
  let [w1,w2] = parseWire <$> lines input
  print $ part2 w1 w2

tests :: [(String, Vec, Either Fuck Int)]
tests = [ ("U1", Vec 0 1, Right 1)
        , ("U1,R3", Vec 1 1, Right 2)
        , ("D1,R3,D2,L1", Vec 3 (-2), Right 5)
        , ("U1,L3", Vec 1 1, Left Fuck)
        ]

runTest :: (String, Vec, Either Fuck Int) -> Bool
runTest (w, p, e) = distAlongWire p (parseWire w) == e
