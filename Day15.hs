
module Day15 where

import Utils(RawInput, coordinateMap, convertCoordinateMap, collectNeighbors)
import qualified Data.Map as Map


day15Input :: IO RawInput
day15Input = do
  inputs <- readFile "day15Input.txt"
  return (lines inputs)

testMain :: IO ()
testMain = do
    rawInput <- day15Input
    print (convertCoordinateMap (coordinateMap rawInput 0 Map.empty))