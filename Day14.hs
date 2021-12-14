module Day14 where

import qualified Data.Map as Map

testTemplate :: [Char]
testTemplate = "NNCB"

type PairRule = (String, Char)

pairRuleLst :: [PairRule]
pairRuleLst =
  [ ("CH", 'B'),
    ("HH", 'N'),
    ("CB", 'H'),
    ("NH", 'C'),
    ("HB", 'C'),
    ("HC", 'B'),
    ("HN", 'C'),
    ("NN", 'C'),
    ("BH", 'H'),
    ("NC", 'B'),
    ("NB", 'B'),
    ("BN", 'B'),
    ("BB", 'N'),
    ("BC", 'B'),
    ("CC", 'N'),
    ("CN", 'C')
  ]

pairRuleMap :: Map.Map String Char
pairRuleMap = Map.fromList pairRuleLst

makePairsOfTemplate :: [Char] -> [(Char, Char)]
makePairsOfTemplate = reverse . go []
  where
    go curPairs [] = curPairs
    go curPairs [_] = curPairs
    go curPairs [cur, neck] = (cur, neck) : curPairs
    go curPairs (cur : neck : rest) = go ((cur, neck) : curPairs) (neck : rest)