module Day9 where
import qualified Data.Map as Map
import Data.Char(digitToInt)

day9Input = do
  inputs <- readFile "day9Input.txt"
  return (lines inputs)


testInput = 
  [
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ]


mapFromArray line = Map.fromList (zip [0..length line] (map digitToInt line))

leftRightNeighbors arrayMap =  map ( \(k,v) -> (v, Map.findWithDefault (-1) (k -1 ) arrayMap, Map.findWithDefault (-1) (k +1 ) arrayMap ) ) (Map.toList (arrayMap))
topBottomNeighbors topArrayMap bottomArrayMap =  map ( \(k,v) -> (Map.findWithDefault (-1) k topArrayMap, Map.findWithDefault (-1) k bottomArrayMap ) ) (Map.toList (bottomArrayMap))

getNeighbors lineAbove line lineBelow = map (\((v,ln,rn),(tn,bn)) -> filter (>=0) [v,ln,rn,tn,bn] ) (zip (lrNeighbors line) (tbNeighbors lineAbove lineBelow)) where
  lrNeighbors line = leftRightNeighbors (mapFromArray line)
  tbNeighbors lineAbove lineBelow = topBottomNeighbors (mapFromArray (lineAbove)) (mapFromArray lineBelow)

iterateForNeighbors [] = []
iterateForNeighbors [head] = getNeighbors [] head []
iterateForNeighbors [head,neck] = getNeighbors head neck []
iterateForNeighbors (head:neck:torso:tail) = 
  (getNeighbors head neck torso) ++ iterateForNeighbors (neck:torso:tail)

lowPoints :: [[Int]] -> [[Int]]
lowPoints = filter ( (\(value:neighbors) -> value < (minimum neighbors)))

  -- let 
  --   lrNeighbors line = leftRightNeighbors (mapFromArray line)
  --   tbNeighbors lineAbove lineBelow = topBottomNeighbors (mapFromArray (lineAbove)) (mapFromArray lineBelow)
  --   allneighborsTransformed = map (\((v,ln,rn),(tn,bn)) -> filter (>=0) [v,ln,rn,tn,bn] ) (zip lrNeighbors tbNeighbors)
  --   lowPoints = map head (filter ( (\(value:neighbors) -> value < (minimum neighbors)))  allneighborsTransformed)
  -- in
  --   lowPoints 