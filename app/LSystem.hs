module Main where

import Test.Tasty
import Test.Tasty.HUnit

data Letter = Letter {
  symbol :: String
} deriving (Eq)

instance Show Letter where
  show (Letter x) = x

type LWord = [Letter]

data Production = Production {
  matchSymbol :: Letter,
  replacement :: LWord
} deriving (Show)

lword = map Letter . words

matchProduction :: [Production] -> Letter -> Production
matchProduction ps l =
  head $ filter (\p -> matchSymbol p == l) ps

step :: LWord -> [Production] -> LWord
step axiom productions =
  concatMap (replacement . matchProduction productions) axiom

stepN 0 axiom _ = axiom
stepN n axiom rules = stepN (n - 1) (step axiom rules) rules

main = defaultMain tests

mkProductions :: [(String, String)] -> [Production]
mkProductions template = map (\(l, w) -> Production {
  matchSymbol = Letter l,
  replacement = lword w
}) template

tests = testGroup "Deterministic & Context-Free (DOL)"
  [ testCase "Trivial" $ (lword "a b a b a a b a") @=? (stepN 5 (lword "b")
      $ mkProductions [
        ("b", "a"),
        ("a", "b a")
      ])
  , testCase "Anabaena catenula" $ (lword "bl ar bl ar ar bl ar ar") @=?
      (stepN 4 (lword "ar")
        $ mkProductions [
          ("ar", "al br"),
          ("al", "bl ar"),
          ("br", "ar"),
          ("bl", "al")
        ])
  ]
