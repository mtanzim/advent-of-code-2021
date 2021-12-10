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

leftNeighbors arrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k -1) arrayMap) ) (Map.toList (arrayMap)))
rightNeighbors arrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k +1) arrayMap) ) (Map.toList (arrayMap)))
topNeighbors topArrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k) topArrayMap) ) (Map.toList (topArrayMap)))
bottomNeighbors bottomArrayMap =  Map.fromList (map ( \(k,v) -> (k, Map.findWithDefault (-1) (k) bottomArrayMap) ) (Map.toList (bottomArrayMap)))

getNeighbors lineAbove line lineBelow =  
  let
    valuesMap = mapFromArray line
    lNeighbors  = leftNeighbors (mapFromArray line)
    rNeighbors = rightNeighbors (mapFromArray line)
    tNeighbors = topNeighbors (mapFromArray lineAbove)
    bNeighbors = bottomNeighbors (mapFromArray lineBelow)
  in
    map (filter (>=0))
    (map (\idx -> 
      [
        Map.findWithDefault (-1) idx valuesMap,
        Map.findWithDefault (-1) idx lNeighbors,
        Map.findWithDefault (-1) idx rNeighbors,
        Map.findWithDefault (-1) idx tNeighbors,
        Map.findWithDefault (-1) idx bNeighbors
      ] )  [0..(length line - 1)])


iterateForNeighbors [] [head] = getNeighbors [] head []
iterateForNeighbors prev [head, neck] = getNeighbors prev head neck ++ getNeighbors head neck []
iterateForNeighbors prev (head:neck:tail) = 
  (getNeighbors prev head neck) ++ iterateForNeighbors head (neck:tail)

lowPoints :: [[Int]] -> [[Int]]
lowPoints = filter ( (\(value:neighbors) -> value < (minimum neighbors)))

testMain = (sum . map (+1)  .map head . lowPoints . iterateForNeighbors []) testInput

riskSum = sum . map (+1)  .map head . lowPoints . iterateForNeighbors []

main = do
  input <- day9Input
  print (riskSum input)
