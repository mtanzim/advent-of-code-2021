module Day9 where

import Data.Char (digitToInt)
import Data.List (sort)
import qualified Data.Map as Map

type Coordinate = (Int, Int)

type CoordinateMap = Map.Map Coordinate Int

type VisitedMap = Map.Map Coordinate Bool

type RawInput = [[Char]]

day9Input :: IO RawInput
day9Input = do
  inputs <- readFile "day9Input.txt"
  return (lines inputs)

main :: IO ()
main = do
  input <- day9Input
  print (getSumOfRisk input)
  print (getProductOfLargestBasins input)

coordinateMap :: RawInput -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
coordinateMap [] _ curMap = curMap
coordinateMap (curLine : rest) lineIdx curMap =
  let mapFromLine = foldr (\charIdx acc -> Map.insert (lineIdx, charIdx) (curLine !! charIdx) acc) curMap [0 .. (length curLine - 1)]
   in coordinateMap rest (lineIdx + 1) mapFromLine

convertCoordinateMap :: Map.Map Coordinate Char -> CoordinateMap
convertCoordinateMap = Map.map digitToInt

collectNeighbors :: Coordinate -> CoordinateMap -> CoordinateMap
collectNeighbors (x, y) coordMap =
  Map.fromList $
    filter
      (\((_, _), v) -> v >= 0)
      [ ((x, y), findFromMap (x, y) coordMap),
        ((x -1, y), findFromMap (x -1, y) coordMap),
        ((x + 1, y), findFromMap (x + 1, y) coordMap),
        ((x, y -1), findFromMap (x, y -1) coordMap),
        ((x, y + 1), findFromMap (x, y + 1) coordMap)
      ]
  where
    findFromMap = Map.findWithDefault (-1)

findConnectedNeighbors :: [Coordinate] -> Map.Map Coordinate Bool -> Map.Map Coordinate Int -> Map.Map Coordinate Bool
findConnectedNeighbors [] visitedMap _ = visitedMap
findConnectedNeighbors (curCoord : rest) visitedMap coordMap =
  let (x, y) = curCoord
      immediateNeighbors =
        filter
          (\((nx, ny), v) -> v < 9 && not (Map.findWithDefault False (nx, ny) visitedMap))
          (Map.toList (collectNeighbors (x, y) coordMap))
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
      let neighbors = Map.filterWithKey (\curCorrd _ -> curCorrd /= coord) (collectNeighbors coord coordMap)
          curVal = Map.findWithDefault (-1) coord coordMap
          neighborVals = Map.elems neighbors
       in curVal < minimum neighborVals

collectBasins :: [Coordinate] -> CoordinateMap -> [Int]
collectBasins toSearch coordMap = map (sizeOfBasin coordMap) toSearch

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

-- DEBUG

testInput :: RawInput
testInput =
  [ "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ]