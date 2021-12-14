module Day14 where

import Data.List.Split
import qualified Data.Map as Map

-- testTemplate :: [Char]
-- testTemplate = "NNCB"

-- type PairRule = (String, Char)

-- testPairRuleList :: [PairRule]
-- testPairRuleList =
--   [ ("CH", 'B'),
--     ("HH", 'N'),
--     ("CB", 'H'),
--     ("NH", 'C'),
--     ("HB", 'C'),
--     ("HC", 'B'),
--     ("HN", 'C'),
--     ("NN", 'C'),
--     ("BH", 'H'),
--     ("NC", 'B'),
--     ("NB", 'B'),
--     ("BN", 'B'),
--     ("BB", 'N'),
--     ("BC", 'B'),
--     ("CC", 'N'),
--     ("CN", 'C')
--   ]

-- testPairRuleMap :: Map.Map String Char
-- testPairRuleMap = Map.fromList testPairRuleList

makePairsOfTemplate :: [Char] -> [(Char, Char)]
makePairsOfTemplate = reverse . go []
  where
    go curPairs [] = curPairs
    go curPairs [_] = curPairs
    go curPairs [cur, neck] = (cur, neck) : curPairs
    go curPairs (cur : neck : rest) = go ((cur, neck) : curPairs) (neck : rest)

applyRuleToPair :: Map.Map String Char -> (Char, Char) -> (Char, Char, Char)
applyRuleToPair ruleMap (start, end) =
  let middle = Map.findWithDefault 'e' [start, end] ruleMap
   in (start, middle, end)

polymerizeOnce :: Map.Map String Char -> [Char] -> [Char]
polymerizeOnce ruleMap input =
  let pipeline =
        concatMap
          ( (\(s, m, _) -> [s, m])
              . applyRuleToPair ruleMap
          )
          . makePairsOfTemplate
   in (++) (pipeline input) [last input]

polymerize :: Int -> Map.Map String Char -> [Char] -> [Char]
polymerize n ruleMap start = foldr (\_ acc -> polymerizeOnce ruleMap acc) start [1 .. n]

makeMapOfOccurence :: String -> Map.Map Char Int
makeMapOfOccurence =
  foldr (\curChar curMap -> Map.insert curChar (getExistingValue curChar curMap) curMap) Map.empty
  where
    getExistingValue curChar' curMap' = (+) 1 $ Map.findWithDefault 0 curChar' curMap'

-- testMain :: Int
-- testMain =
--   let occurenceMap = makeMapOfOccurence (polymerize 10 testPairRuleMap testTemplate)
--       values = Map.elems occurenceMap
--       minOcc = minimum values
--       maxOcc = maximum values
--    in maxOcc - minOcc

-- day14Input :: IO [String]
day14Input :: IO (String, [([Char], Char)])
day14Input = do
  inputs <- readFile "day14Input.txt"
  return (getTemplate (lines inputs), getRuleList (lines inputs))
  where
    getTemplate = head
    getRuleList (_ : _ : rest) = map ((\[k, v] -> (k, head v)) . splitOn " -> ") rest
    getRuleList _ = []

-- main :: IO Int
main :: IO Int
main = do
  (template, ruleLst) <- day14Input
  return (mainFn template (Map.fromList ruleLst))

-- mainFn :: String -> Map.Map String Char -> Int
mainFn :: [Char] -> Map.Map String Char -> Int
mainFn template ruleMap =
  let occurenceMap = makeMapOfOccurence (polymerize 40 ruleMap template)
      values = Map.elems occurenceMap
      minOcc = minimum values
      maxOcc = maximum values
   in maxOcc - minOcc