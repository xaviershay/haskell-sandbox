-- Quick & dirty solutions to Advent of Code. Many of these are unsafe/partial
-- functions, particularly in the case of unexpected/erroneous input.
--
-- No consideration is given to runtime performance.
module Main where

import Aoc2023.Day1
import Aoc2023.Day2
import Aoc2023.Day3

import Test.Tasty

main = defaultMain tests
tests = testGroup "Days"
  [ day1
  , day2
  , day3
  ]
