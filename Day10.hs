module Day10 where

import qualified Data.Map as Map

type RawInput = [String]

day10Input :: IO RawInput
day10Input = do
  inputs <- readFile "day10Input.txt"
  return (lines inputs)

openToCloseLst :: [(Char, Char)]
openToCloseLst =
  [ ('{', '}'),
    ('(', ')'),
    ('[', ']'),
    ('<', '>')
  ]

openToCloseMap :: Map.Map Char Char
openToCloseMap = Map.fromList openToCloseLst

closeToOpenLst :: [(Char, Char)]
closeToOpenLst = map (\(open, close) -> (close, open)) openToCloseLst

closeToOpenMap :: Map.Map Char Char
closeToOpenMap = Map.fromList closeToOpenLst

traverseLine :: [Char] -> [Char] -> [Char]
traverseLine curStack [] = curStack
traverseLine [] (curChar : rest) = traverseLine [curChar] rest
traverseLine (topOfStack : restOfStack) (curChar : rest) =
  let isCloser = Map.member curChar closeToOpenMap
      expectedCloser = Map.findWithDefault 'e' topOfStack openToCloseMap
   in if isCloser
        then
          if curChar == expectedCloser
            then traverseLine restOfStack rest
            else topOfStack : restOfStack
        else traverseLine (curChar : topOfStack : restOfStack) rest

testInput :: [Char]
testInput = "{([(<{}[<>[]}>{[]{[(<()>"

testMain :: [Char]
testMain = traverseLine [] testInput