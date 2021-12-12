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

data Expected = Expected Char deriving (Eq, Show)

data Found = Found Char deriving (Eq, Show)

traverseLine :: [Char] -> [Char] -> Maybe (Expected, Found)
traverseLine _ [] = Nothing
traverseLine [] (curChar : rest) = traverseLine [curChar] rest
traverseLine (topOfStack : restOfStack) (curChar : rest) =
  let isCloser = Map.member curChar closeToOpenMap
      expectedCloser = Map.findWithDefault 'e' topOfStack openToCloseMap
   in if isCloser
        then
          if curChar == expectedCloser
            then traverseLine restOfStack rest
            else Just (Expected expectedCloser, Found curChar)
        else traverseLine (curChar : topOfStack : restOfStack) rest

testInput :: [String]
testInput =
  [ "{([(<{}[<>[]}>{[]{[(<()>",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "[<>({}){}[([])<>]]"
  ]

calculateSyntaxScore :: [[Char]] -> [Maybe (Expected, Found)]
calculateSyntaxScore = filter onlyCorrupted . map (traverseLine [])
  where
    onlyCorrupted Nothing = False
    onlyCorrupted (Just _) = True

main :: IO ()
main = do
  input <- day10Input
  print (calculateSyntaxScore input)

testMain :: [Maybe (Expected, Found)]
testMain = map (traverseLine []) testInput