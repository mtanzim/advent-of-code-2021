module Day9 where
import qualified Data.Map as Map
import Data.Char(digitToInt)

day9Input = do
  inputs <- readFile "day9Input.txt"
  return (lines inputs)


testInput = 
  [
    "2199943210",
    "3987894921"
    -- "9856789892",
    -- "8767896789",
    -- "9899965678"
  ]


mapFromArray line = Map.fromList (zip [0..length line] (map digitToInt line))

leftNeighbors arrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k -1) arrayMap) ) (Map.toList (arrayMap)))
rightNeighbors arrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k +1) arrayMap) ) (Map.toList (arrayMap)))
topNeighbors topArrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k) topArrayMap) ) (Map.toList (topArrayMap)))
bottomNeighbors bottomArrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k) bottomArrayMap) ) (Map.toList (bottomArrayMap)))
-- topNeighbors topArrayMap =  map ( \(k,v) -> (v,Map.findWithDefault (-1) k topArrayMap )) (Map.toList (topArrayMap))
-- bottomNeighbors bottomArrayMap =  map ( \(k,v) -> (v,Map.findWithDefault (-1) k bottomArrayMap ) ) (Map.toList (bottomArrayMap))

-- map (\entries -> filter (>=0) entries ) 

-- argh the zip is the problem
getNeighbors lineAbove line lineBelow = (mapFromArray line, lNeighbors line, rNeighbors line, tNeighbors lineAbove, bNeighbors lineBelow) where
  lNeighbors line = leftNeighbors (mapFromArray line)
  rNeighbors line = rightNeighbors (mapFromArray line)
  tNeighbors lineAbove = topNeighbors (mapFromArray lineAbove)
  bNeighbors lineBelow = bottomNeighbors (mapFromArray lineBelow)

-- iterateForNeighbors [] [head] = getNeighbors [] head []
-- iterateForNeighbors prev [head, neck] = getNeighbors prev head neck ++ getNeighbors head neck []
-- iterateForNeighbors prev (head:neck:tail) = 
--   (getNeighbors prev head neck) ++ iterateForNeighbors head (neck:tail)

-- lowPoints :: [[Int]] -> [[Int]]
-- lowPoints = filter ( (\(value:neighbors) -> value < (minimum neighbors)))

  -- let 
  --   allneighborsTransformed = map (\((v,ln,rn),(tn,bn)) -> filter (>=0) [v,ln,rn,tn,bn] ) (zip lrNeighbors tbNeighbors)
  --   lowPoints = map head (filter ( (\(value:neighbors) -> value < (minimum neighbors)))  allneighborsTransformed)
  -- in
  --   lowPoints 