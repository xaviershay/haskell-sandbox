module Aoc2023.Day1 where

import Test.Tasty
import Test.Tasty.HUnit

import Data.List (isPrefixOf, find)

import Control.Arrow (first, (&&&))

day1 = testGroup "Day 1"
    [ testCase "Example" $ do
        x <- calibrate numbersOnly "data/aoc2023/day1/example.txt"
        x @?= 142
    , testCase "Input" $ do
        x <- calibrate numbersOnly "data/aoc2023/day1/input.txt"
        x @?= 56049
    , testCase "Part 2" $ do
        x <- calibrate numbersAndWords "data/aoc2023/day1/input.txt"
        x @?= 54530
    , testCase "seekDigit" $
        seekDigit numbersAndWords "aaonexxxtwobb" @?= 1
    , testCase "seekDigit reversed" $
        seekDigit
          (reverseMappings numbersAndWords)
          (reverse "aaonexxxtwobb") @?= 2
    ]
  where
    calibrate values filename =
      sum . map calibrationValue . lines <$> readFile filename

      where
        calibrationValue :: String -> Int
        calibrationValue input =
          seekDigit values input * 10 +
          seekDigit (reverseMappings values) (reverse input)

    seekDigit :: [(String, Int)] -> String -> Int
    seekDigit mappings "" = error "no digit found"
    seekDigit mappings input =
      case find (\(label, _) -> isPrefixOf label input) mappings of
        Just (_, v) -> v
        Nothing     -> seekDigit mappings (drop 1 input)

    reverseMappings = map (first reverse)

    numbersOnly = map (show &&& id) [0..9]

    numbersAndWords =
      [ ("one", 1)
      , ("two", 2)
      , ("three", 3)
      , ("four", 4)
      , ("five", 5)
      , ("six", 6)
      , ("seven", 7)
      , ("eight", 8)
      , ("nine", 9)
      ] ++ numbersOnly
