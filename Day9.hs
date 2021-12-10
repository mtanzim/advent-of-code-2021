module Day9 where
import qualified Data.Map as Map
import Data.Char(digitToInt)

day9Input = do
  inputs <- readFile "day9Input.txt"
  return (lines inputs)


testInput = ["2199943210","3987894921","9856789892","8767896789","9899965678"]


mapFromArray line = Map.fromList (zip [0..length line] (map digitToInt line))

leftRightNeighbors arrayMap =  map ( \(k,v) -> (Map.lookup (k -1 ) arrayMap, v, Map.lookup (k +1 ) arrayMap ) ) (Map.toList (arrayMap))