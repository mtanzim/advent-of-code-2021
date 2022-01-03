
module Day15 where

import Utils(RawInput, Coordinate, coordinateMap, convertCoordinateMap, collectNeighbors)
import qualified Data.Map as Map

data EdgeTo = EdgeTo {
    from :: Coordinate,
    to:: Coordinate
} deriving (Eq, Show)

data TrackerInfo = TrackerInfo {
    distTo :: Int,
    edgeTo :: EdgeTo
} deriving (Eq, Show)

-- type Tracker = Map.Map Coordinate TrackerInfo

day15Input :: IO RawInput
day15Input = do
  inputs <- readFile "day15Input.txt"
  return (lines inputs)


-- testMain :: Coordinate -> IO ()
testMain coord = do
    rawInput <- day15Input
    let 
        coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
        neighbors = collectNeighbors coord coordMap
        -- updatedTracker :: Tracker
        updatedTracker = foldr fn Map.empty neighbors where
            fn ((x,y), risk) acc = Map.insert (x,y) TrackerInfo{distTo=risk, edgeTo=EdgeTo{from=coord,to=(x,y)}} acc
     in do
         print (neighbors)