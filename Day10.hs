module Day10 where

import Data.List (sort)
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

openToAutoCompletionScore :: [(Char, Int)]
openToAutoCompletionScore =
  [ (')', 1),
    (']', 2),
    ('}', 3),
    ('>', 4)
  ]

openToAutoCompletionScoreMap :: Map.Map Char Int
openToAutoCompletionScoreMap = Map.fromList openToAutoCompletionScore

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

data RemainingStack = RemainingStack String deriving (Eq, Show)

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

calculateSyntaxScore :: [[Char]] -> Int
calculateSyntaxScore = sum . map scoreErrors . filter onlyCorrupted . map (traverseLine [])
  where
    onlyCorrupted (Right _) = False
    onlyCorrupted (Left _) = True
    scoreErrors (Left (Expected _, Found c)) = Map.findWithDefault 0 c closeToErrorScoreMap
    scoreErrors _ = 0

calculateAutoCompleteScores :: [[Char]] -> Int
calculateAutoCompleteScores =
  getMiddleScore
    . sort
    . map getTotalScore
    . map getAutoCompletionScores
    . map getAutoCompletions
    . filter onlyIncomplete
    . map (traverseLine [])
  where
    onlyIncomplete (Right _) = True
    onlyIncomplete (Left _) = False
    getAutoCompletions (Right (RemainingStack s)) = map (\c -> Map.findWithDefault 'e' c openToCloseMap) s
    getAutoCompletions _ = ""
    getAutoCompletionScores = map (\c -> Map.findWithDefault 0 c openToAutoCompletionScoreMap)
    getTotalScore = foldl (\acc c -> (acc * 5) + c) 0
    getMiddleScore scores =
      let midIndex = length scores `div` 2
       in scores !! midIndex

main :: IO ()
main = do
  input <- day10Input
  print (calculateSyntaxScore input)
  print (calculateAutoCompleteScores input)

-- testMain :: [Result]
testMain = calculateAutoCompleteScores testInput

testInput :: [String]
testInput =
  [ "{([(<{}[<>[]}>{[]{[(<()>",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "[<>({}){}[([])<>]]",
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>("
  ]