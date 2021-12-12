module Day9 where

import Data.Char (digitToInt)
import Data.List
import qualified Data.Map as Map

day9Input :: IO [String]
day9Input = do
  inputs <- readFile "day9Input.txt"
  return (lines inputs)

type RawInput = [[Char]]

testInput :: RawInput
testInput =
  [ "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
  ]

mapFromArray :: [Char] -> Map.Map Int Int
mapFromArray line = Map.fromList (zip [0 .. length line] (map digitToInt line))

leftNeighbors :: (Ord k, Num a, Num k) => Map.Map k a -> Map.Map k a
leftNeighbors arrayMap = Map.fromList (map (\(k, v) -> (k, Map.findWithDefault (-1) (k -1) arrayMap)) (Map.toList arrayMap))

rightNeighbors arrayMap = Map.fromList (map (\(k, v) -> (k, Map.findWithDefault (-1) (k + 1) arrayMap)) (Map.toList arrayMap))

topNeighbors topArrayMap = Map.fromList (map (\(k, v) -> (k, Map.findWithDefault (-1) k topArrayMap)) (Map.toList topArrayMap))

bottomNeighbors bottomArrayMap = Map.fromList (map (\(k, v) -> (k, Map.findWithDefault (-1) k bottomArrayMap)) (Map.toList bottomArrayMap))

getNeighbors :: [Char] -> [Char] -> [Char] -> [[Int]]
getNeighbors lineAbove line lineBelow =
  let valuesMap = mapFromArray line
      lNeighbors = leftNeighbors (mapFromArray line)
      rNeighbors = rightNeighbors (mapFromArray line)
      tNeighbors = topNeighbors (mapFromArray lineAbove)
      bNeighbors = bottomNeighbors (mapFromArray lineBelow)
   in map
        ( filter (>= 0)
            . ( \idx ->
                  [ Map.findWithDefault (-1) idx valuesMap,
                    Map.findWithDefault (-1) idx lNeighbors,
                    Map.findWithDefault (-1) idx rNeighbors,
                    Map.findWithDefault (-1) idx tNeighbors,
                    Map.findWithDefault (-1) idx bNeighbors
                  ]
              )
        )
        [0 .. (length line - 1)]

iterateForNeighbors :: [Char] -> [[Char]] -> [[Int]]
iterateForNeighbors [] [] = []
iterateForNeighbors [] [head] = getNeighbors [] head []
iterateForNeighbors prev [] = getNeighbors prev [] []
iterateForNeighbors prev [head, neck] = getNeighbors prev head neck ++ getNeighbors head neck []
iterateForNeighbors prev (head : neck : tail) =
  getNeighbors prev head neck ++ iterateForNeighbors head (neck : tail)

lowPoints :: [[Int]] -> [[Int]]
lowPoints = filter (\(value : neighbors) -> value < minimum neighbors)

testMain :: Int
testMain = (sum . map ((+ 1) . head) . lowPoints . iterateForNeighbors []) testInput

riskSum :: [[Char]] -> Int
riskSum = sum . map ((+ 1) . head) . lowPoints . iterateForNeighbors []

-- main :: IO ()
-- main = do
--   input <- day9Input
--   print (riskSum input)

-- for part B

coordinateMap :: [String] -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
coordinateMap [] _ curMap = curMap
coordinateMap (curLine : tail) lineIdx curMap =
  let mapFromLine = foldr (\charIdx acc -> Map.insert (lineIdx, charIdx) (curLine !! charIdx) acc) curMap [0 .. (length curLine - 1)]
   in coordinateMap tail (lineIdx + 1) mapFromLine

convertCoordinateMap :: Map.Map k Char -> Map.Map k Int
convertCoordinateMap = Map.map digitToInt

type Coordinate = (Int, Int)

type NeighborMap = Map.Map Coordinate Int

type VisitedMap = Map.Map Coordinate Bool

collectNeighbors :: (Int, Int) -> NeighborMap -> NeighborMap
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

sizeOfBasin :: NeighborMap -> Coordinate -> Int
sizeOfBasin coordMap startCoord =
  let connectedNeighbors = findConnectedNeighbors [startCoord] (Map.insert startCoord True Map.empty) coordMap
      valuesOfConnectedNeighbors = map (\(x, y) -> Map.lookup (x, y) coordMap) (Map.keys connectedNeighbors)
   in length valuesOfConnectedNeighbors

-- getImmediateNeighbors :: NeighborMap -> [NeighborMap]
collectLowPoints coordMap = filter fn (Map.keys coordMap)
  where
    fn coord =
      let neighbors = collectNeighbors coord coordMap
          curVal = Map.findWithDefault (-1) coord coordMap
          neighborVals = Map.elems neighbors
       in curVal <= minimum neighborVals

-- collectBasins :: RawInput -> [Int]
collectBasins :: [Coordinate] -> NeighborMap -> [Int]
collectBasins toSearch coordMap = map (sizeOfBasin coordMap) toSearch

testMain' = collectBasins [(0, 1), (0, 9), (2, 2), (4, 6)]

getProductOfLargestBasins :: RawInput -> Int
getProductOfLargestBasins rawInput =
  let coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
      lowPoints = collectLowPoints coordMap
      productOfLargestBasins = foldr (*) 1 . take 3 . reverse . sort
   in productOfLargestBasins (collectBasins lowPoints coordMap)

main :: IO ()
main = do
  input <- day9Input
  print (riskSum input)
  print (getProductOfLargestBasins input)
