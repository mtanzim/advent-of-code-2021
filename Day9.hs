module Day9 (day9Main) where

import Data.Char (digitToInt)
import Data.List (sort)
import qualified Data.Map as Map
import Utils(coordinateMap, collectNeighbors, RawInput, convertCoordinateMap, Coordinate, CoordinateMap)


type VisitedMap = Map.Map Coordinate Bool


day9Input :: IO RawInput
day9Input = do
  inputs <- readFile "day9Input.txt"
  return (lines inputs)

day9Main :: IO ()
day9Main = do
  input <- day9Input
  print (getSumOfRisk input)
  print (getProductOfLargestBasins input)

getSumOfRisk :: RawInput -> Int
getSumOfRisk rawInput =
  let coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
      lowPointCoords = collectLowPoints coordMap
      lowPointValues = map ((+ 1) . (\coord -> Map.findWithDefault (-1) coord coordMap)) lowPointCoords
   in sum lowPointValues

getProductOfLargestBasins :: RawInput -> Int
getProductOfLargestBasins rawInput =
  let coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
      lowPointCoords = collectLowPoints coordMap
      productOfLargestBasins = product . take 3 . reverse . sort
   in productOfLargestBasins (collectBasins lowPointCoords coordMap)

-- Custom BFS FP Style :yay:
findConnectedNeighbors :: [Coordinate] -> VisitedMap -> CoordinateMap -> VisitedMap
findConnectedNeighbors [] visitedMap _ = visitedMap
findConnectedNeighbors (curCoord : rest) visitedMap coordMap =
  let (x, y) = curCoord
      immediateNeighbors =
        filter
          predicateFn
          (Map.toList (collectNeighbors (x, y) coordMap))
        where
          predicateFn ((nx, ny), v) = v < 9 && not (Map.findWithDefault False (nx, ny) visitedMap)
      immediateNeighborCoords = map fst immediateNeighbors
      updatedQueue = rest ++ immediateNeighborCoords
      updatedVisited = foldr (\(curX, curY) curMap -> Map.insert (curX, curY) True curMap) visitedMap immediateNeighborCoords
   in findConnectedNeighbors updatedQueue updatedVisited coordMap

sizeOfBasin :: CoordinateMap -> Coordinate -> Int
sizeOfBasin coordMap startCoord =
  let connectedNeighbors = findConnectedNeighbors [startCoord] (Map.insert startCoord True Map.empty) coordMap
      valuesOfConnectedNeighbors = map (\(x, y) -> Map.lookup (x, y) coordMap) (Map.keys connectedNeighbors)
   in length valuesOfConnectedNeighbors

collectLowPoints :: CoordinateMap -> [Coordinate]
collectLowPoints coordMap = filter fn (Map.keys coordMap)
  where
    fn coord =
      let neighbors = Map.filterWithKey (\curCoord _ -> curCoord /= coord) (collectNeighbors coord coordMap)
          curVal = Map.findWithDefault (-1) coord coordMap
          neighborVals = Map.elems neighbors
       in curVal < minimum neighborVals

collectBasins :: [Coordinate] -> CoordinateMap -> [Int]
collectBasins toSearch coordMap = map (sizeOfBasin coordMap) toSearch

-- DEBUG

testInput :: RawInput
testInput =
  [ "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ]

testMainA :: Int
testMainA = getSumOfRisk testInput

testMainB :: Int
testMainB = getProductOfLargestBasins testInput