
module Day15 where

import Utils(RawInput, Coordinate, CoordinateMap, coordinateMap, convertCoordinateMap, collectNeighbors)
import qualified Data.Map as Map

data EdgeTo = EdgeTo {
    from :: Coordinate,
    to:: Coordinate
} deriving (Eq, Show)

data TrackerInfo = TrackerInfo {
    distTo :: Int,
    edgeTo :: EdgeTo
} deriving (Eq, Show)

type Tracker = Map.Map Coordinate TrackerInfo

day15Input :: IO RawInput
day15Input = do
  inputs <- readFile "day15Input.txt"
  return (lines inputs)


-- testMain :: Coordinate -> IO ()
testMain coord = do
    rawInput <- day15Input
    let 
        coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
        neighbors :: CoordinateMap
        neighbors = collectNeighbors coord coordMap
        neighborsLst = Map.toList neighbors
        closestNeighbor :: (Coordinate, Int)
        closestNeighbor =  foldr findMin (head neighborsLst) neighborsLst where 
            findMin (curCoord, risk) (minCoord, minRisk) = 
                if risk < minRisk 
                    then (curCoord, risk) 
                    else (minCoord, minRisk)
        updatedTracker :: Tracker
        updatedTracker = foldr fn Map.empty neighborsLst where
            fn (curCoord, risk) acc = 
                Map.insert 
                    curCoord
                    TrackerInfo {
                        distTo=risk, 
                        edgeTo=
                            EdgeTo {from=coord,to=curCoord}
                    } 
                    acc
     in do
         print (neighbors)
         print (closestNeighbor)
         print (updatedTracker)