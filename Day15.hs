
module Day15(day15Main) where

import Utils(RawInput, Coordinate, CoordinateMap, coordinateMap, convertCoordinateMap, collectNeighbors)
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQMin
import Data.Maybe

data Vertex = Vertex Coordinate Int deriving (Eq, Show)

instance Ord Vertex where
    compare (Vertex _ distA) (Vertex _ distB)  = compare distA distB

day15Input :: IO RawInput
day15Input = do
  inputs <- readFile "day15Input.txt"
  return (lines inputs)

day15Main :: IO ()
day15Main = do
    rawInput <- day15Input
    let coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
    print (findLeastRiskDjikstra coordMap (0,0))

expandSingleCoord :: Coordinate -> Int -> [Coordinate]
expandSingleCoord (x,y) size =
    let 
        newCoordOneDimension = map (*size) [1..2]
        xs = x : map (+x) newCoordOneDimension
        ys = y : map (+y) newCoordOneDimension
        newCoord = [(x,y) | x <- xs, y <- ys]
    in 
        newCoord

expandCoordMap :: Map.Map Coordinate Int -> Int -> Map.Map Coordinate Int
expandCoordMap coordMap size =
    foldr fn coordMap (Map.toList coordMap) where
        fn (curCoord, _) acc =
            let 
                expandedCoord = tail $ expandSingleCoord curCoord size
                updatedMap = foldr fn' acc expandedCoord where
                    fn' (curX, curY) acc' = Map.insert (curX, curY) (-1) acc'
            in
                updatedMap

fillRisks :: Map.Map Coordinate Int -> Int -> Map.Map Coordinate Int
fillRisks coordMap size = go coordMap (Map.toList coordMap) where
    go cm [] = cm
    go cm (((x,y), risk):rest) = if risk < 0 then
        let
            prevX = x - size
            prevY = y - size
            riskY = Map.findWithDefault (-1) (x,prevY) cm
            riskX = Map.findWithDefault (-1) (prevX,y) cm
            riskUpdated = max riskX riskY
            um = Map.insert (x,y) riskUpdated cm
        in
            go um rest
            
    else
        go cm rest



test = do
    rawInput <- day15Input
    let 
        coordMap = convertCoordinateMap (coordinateMap rawInput 0 Map.empty)
        size = length rawInput
        expanded = expandCoordMap coordMap size
    -- print size
    print expanded
    print $ fillRisks expanded size
    
-- based on: https://algs4.cs.princeton.edu/44sp/DijkstraSP.java.html
findLeastRiskDjikstra :: Map.Map Coordinate Int -> Coordinate -> Int
findLeastRiskDjikstra coordMap source = do
    let 
        -- initialize
        edgeTo :: Map.Map Coordinate Coordinate
        edgeTo = Map.empty
        distTo :: Map.Map Coordinate Int
        distTo' = foldr fn Map.empty (Map.toList coordMap) where
            fn (curCoord, _) acc = Map.insert curCoord (maxBound :: Int) acc
        distTo = Map.insert source 0 distTo'
        visited :: Map.Map Coordinate Bool
        visited = foldr fnVisit Map.empty (Map.toList coordMap) where
            fnVisit (curCoord, _) acc = Map.insert curCoord False acc
        pq :: PQMin.MinQueue Vertex
        pq = PQMin.singleton (Vertex source 0)

        -- iterate over priority queue
        -- TODO: edgeTo tracking for the actual path
        -- wait, the questions do not require it?
        in go pq edgeTo distTo visited where
            go q et dt vst = 
                if PQMin.null q then
                    -- findMax finds the bottom right point
                    -- the value of this is the shortest weighted distance from source
                    snd $ Map.findMax dt
                else
                    let 
                        Vertex fromCoord _ = PQMin.findMin q
                        vst' = Map.insert fromCoord True vst
                        neighbors :: CoordinateMap
                        neighbors = collectNeighbors fromCoord coordMap
                        filteredNeighborsLst = Map.toList $ Map.filterWithKey (\k _ -> not $ Map.findWithDefault False k vst') neighbors
                        zero = (PQMin.deleteMin q, et, dt)
                        -- iterate over neighbors
                        (uq, uet, udt) = foldr relax zero filteredNeighborsLst where
                            relax (toCoord, weight) (q', et', dt') =
                                let
                                    fromDistTo = Map.findWithDefault 0 fromCoord dt'
                                    toDistTo = Map.findWithDefault 0 toCoord dt'
                                    minDistTo = min toDistTo (fromDistTo + weight)
                                    updatedDistTo = Map.insert toCoord minDistTo dt'
                                    -- curEdgeTo = Map.findWithDefault toCoord fromCoord et'
                                    -- updatedCurEdgeTo = if minDistTo < toDistTo then toCoord else curEdgeTo
                                    -- updatedEdgeTo = Map.mapWithKey uetf et' where
                                    --     uetf k v = if k == fromCoord then updatedCurEdgeTo else v
                                    filteredQ = PQMin.filter (\(Vertex coord' _) -> toCoord /= coord') q'
                                    updatedQ = PQMin.insert (Vertex toCoord minDistTo) filteredQ
                                in
                                    (updatedQ, Map.empty, updatedDistTo)
                        in go uq uet udt vst'
    
