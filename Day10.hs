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

closeToErrorScore :: [(Char, Int)]
closeToErrorScore =
  [ ('}', 1197),
    (')', 3),
    (']', 57),
    ('>', 25137)
  ]

closeToErrorScoreMap :: Map.Map Char Int
closeToErrorScoreMap = Map.fromList closeToErrorScore

openToCloseMap :: Map.Map Char Char
openToCloseMap = Map.fromList openToCloseLst

closeToOpenLst :: [(Char, Char)]
closeToOpenLst = map (\(open, close) -> (close, open)) openToCloseLst

closeToOpenMap :: Map.Map Char Char
closeToOpenMap = Map.fromList closeToOpenLst

data Expected = Expected Char deriving (Eq, Show)

data Found = Found Char deriving (Eq, Show)

data RemainingStack = RemainingStack String

type Result = Either (Expected, Found) RemainingStack

traverseLine :: [Char] -> [Char] -> Result
traverseLine curStack [] = Right (RemainingStack curStack)
traverseLine [] (curChar : rest) = traverseLine [curChar] rest
traverseLine (topOfStack : restOfStack) (curChar : rest) =
  let isCloser = Map.member curChar closeToOpenMap
      expectedCloser = Map.findWithDefault 'e' topOfStack openToCloseMap
   in if isCloser
        then
          if curChar == expectedCloser
            then traverseLine restOfStack rest
            else Left (Expected expectedCloser, Found curChar)
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

calculateSyntaxScore :: [[Char]] -> Int
calculateSyntaxScore = sum . map scoreErrors . filter onlyCorrupted . map (traverseLine [])
  where
    onlyCorrupted (Right _) = False
    onlyCorrupted (Left _) = True
    scoreErrors (Left (Expected _, Found c)) = Map.findWithDefault 0 c closeToErrorScoreMap

main :: IO ()
main = do
  input <- day10Input
  print (calculateSyntaxScore input)

testMain :: [Maybe (Expected, Found)]
testMain = map (traverseLine []) testInput