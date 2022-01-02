module Utils where

import qualified Data.Map as Map
import Data.Char (digitToInt)


type RawInput = [String]
type Coordinate = (Int, Int)

type CoordinateMap = Map.Map Coordinate Int

convertCoordinateMap :: Map.Map Coordinate Char -> CoordinateMap
convertCoordinateMap = Map.map digitToInt

coordinateMap :: RawInput -> Int -> Map.Map (Int, Int) Char -> Map.Map (Int, Int) Char
coordinateMap [] _ curMap = curMap
coordinateMap (curLine : rest) lineIdx curMap =
  let mapFromLine = foldr (\charIdx acc -> Map.insert (lineIdx, charIdx) (curLine !! charIdx) acc) curMap [0 .. (length curLine - 1)]
   in coordinateMap rest (lineIdx + 1) mapFromLine

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