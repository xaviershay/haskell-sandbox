{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.String( IsString(..) )

data Letter = Letter {
  symbol :: Char
} deriving (Eq)

instance Show Letter where
  show (Letter x) = [x]

type LWord = [Letter]

data Production = Production {
  matchSymbol :: Letter,
  replacement :: LWord
} deriving (Show)

lword = map Letter

a = Letter 'A'
b = Letter 'B'

rules = [
    Production {
      matchSymbol = Letter 'B',
      replacement = [a]
    },
    Production {
      matchSymbol = Letter 'A',
      replacement = [a, b]
    }
  ]


matchProduction :: [Production] -> Letter -> Production
matchProduction ps l =
  head $ filter (\p -> matchSymbol p == l) ps

step :: LWord -> [Production] -> LWord
step axiom productions =
  concatMap (replacement . matchProduction productions) axiom

stepN 0 axiom _ = axiom
stepN n axiom rules = stepN (n - 1) (step axiom rules) rules

main = defaultMain tests
-- putStrLn . show $ stepN 4 [a] rules

mkProductions :: [(Char, String)] -> [Production]
mkProductions template = map (\(l, w) -> Production { matchSymbol = Letter l, replacement = lword w }) template

tests = testGroup "LSystem"
  [ testCase "DOL trivial" $ (lword "ababaaba") @=? (stepN 5 (lword "b")
      $ mkProductions [
        ('b', "a"),
        ('a', "ba")
      ])
  ]
