-- Quick & dirty solutions to Advent of Code. Many of these are unsafe/partial
-- functions, particularly in the case of unexpected/erroneous input.
--
-- No consideration is given to runtime performance.
module Main where

import Debug.Trace
import Data.Char (isDigit)
import Data.List
import qualified Data.HashMap.Strict as M
import Control.Arrow (first, (&&&))

import Text.Parsec

import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests
tests = testGroup "Days"
  [ day1
  , day2
  ]

parseUnsafe :: Parsec SourceName () a -> SourceName -> a
parseUnsafe parser input =
  case parse parser input input of
    Right x -> x
    Left x -> error $ show x

type Bag = M.HashMap String Int

day2 = testGroup "Day 2"
  [ testCase "Example" $ do
        x <- calculateValid "data/aoc2023/day2/example.txt"
        x @?= 8
  , testCase "Input" $ do
        x <- calculateValid "data/aoc2023/day2/input.txt"
        x @?= 2512
  , testCase "Part 2 Example" $ do
        x <- calculatePower "data/aoc2023/day2/example.txt"
        x @?= 2286
  , testCase "Part 2 Input" $ do
        x <- calculatePower "data/aoc2023/day2/input.txt"
        x @?= 67335
  ]
  where
    calculatePower filename =
      sum
      . map power
      . map (parseUnsafe game)
      . lines <$> readFile filename
      
    calculateValid filename =
      sum
      . map fst
      . filter (validForBag $
          M.fromList [("red", 12), ("green", 13), ("blue", 14)])
      . map (parseUnsafe game)
      . lines <$> readFile filename

    validForBag :: Bag -> (Int, [Bag]) -> Bool
    validForBag bag (_, draws) =
      all (all (>= 0))
      . map M.elems
      . map (\draw -> M.unionWith (-) bag draw)
      $ draws

    power :: (Int, [Bag]) -> Int
    power (_, draws) =
      let minBag = foldl'
                     (\m x -> M.unionWith max m x)
                     (M.fromList [("red", 0), ("green", 0), ("blue", 0)]) 
                     draws
      in

      foldl' (*) 1 . M.elems $ minBag

    -- Parsers
    colorResult = do
      n <- read <$> many1 digit
      string " "
      color <- string "red" <|> string "green" <|> string "blue"

      return (color, n)

    draw = M.fromList <$> colorResult `sepBy1` (string ", ")

    game = do
       string "Game "
       gameId <- read <$> many1 digit
       string ": "
     
       drawResult <- draw `sepBy1` string "; "

       return (gameId, drawResult)

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

