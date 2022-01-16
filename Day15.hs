
module Day15 where

import Utils(RawInput, Coordinate, CoordinateMap, coordinateMap, convertCoordinateMap, collectNeighbors)
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQMin
import Data.Maybe


data TrackerInfo = TrackerInfo {
    distTo :: Int,
    edgeTo :: EdgeTo
} deriving (Eq, Show)

data EdgeTo = EdgeTo {
    from :: Coordinate,
    to:: Coordinate
} deriving (Eq, Show)


type Tracker = Map.Map Coordinate TrackerInfo

data Vertex = Vertex Coordinate Int deriving (Eq, Show)

instance Ord Vertex where
    compare (Vertex _ distA) (Vertex _ distB)  = compare distA distB

day15Input :: IO RawInput
day15Input = do
  inputs <- readFile "day15Input.txt"
  return (lines inputs)


djikstra = do
    rawInput <- day15Input
    let 
        coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
        edgeTo :: Map.Map Coordinate Coordinate
        edgeTo = Map.empty
        distTo :: Map.Map Coordinate Int
        distTo = foldr fn Map.empty (Map.toList coordMap) where
            fn (curCoord, _) acc = Map.insert curCoord (maxBound :: Int) acc
        source :: Coordinate
        source = (0,0)
        pq :: PQMin.MinQueue Vertex
        pq = PQMin.singleton (Vertex source 0)
        rv = go pq edgeTo distTo where
            go q et dt = 
                let 
                    Vertex v _ = PQMin.findMin q
                    neighbors :: CoordinateMap
                    neighbors = collectNeighbors v coordMap
                    filteredNeighborsLst = Map.toList $ Map.filterWithKey (\k _ -> k /= v) neighbors
                    zero = (q, et, dt)
                in
                    foldr relax zero filteredNeighborsLst where
                        relax (coord, weight) (q', et', dt') =
                            let
                                (from, to) = coord
                                curDist = Map.findWithDefault 0 coord dt'
                                newDist = curDist + weight
                                updatedCurDist = min curDist newDist
                                curEdgeTo = Map.findWithDefault coord coord et'
                                updatedCurEdgeTo = if newDist < curDist then coord else curEdgeTo
                                filteredQ = PQMin.filter (\(Vertex coord' dist) -> coord /= coord') q'
                                
                                updatedQ = PQMin.insert (Vertex coord updatedCurDist) filteredQ
                                updatedDistTo = Map.mapWithKey udtf dt' where
                                    udtf k v = if k == coord then updatedCurDist else v
                                updatedEdgeTo = Map.mapWithKey uetf et' where
                                    uetf k v = if k == coord then updatedCurEdgeTo else v
                            in
                                (updatedQ, updatedEdgeTo, updatedDistTo)






        -- go pq distTo
    print (PQMin.toList pq)
    print (distTo)
    print (rv)
    




testMain :: Coordinate -> IO ()
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
     
    print (neighbors)
    print (closestNeighbor)
    print (updatedTracker)