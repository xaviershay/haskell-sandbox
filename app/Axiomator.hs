{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace (trace, traceM)
import Data.List
import Data.Hashable
import GHC.Generics
import qualified Data.Tuple
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Monoid ((<>))
import Control.Monad (msum, forM_)
import Text.Parsec( runParser, char, (<|>), ParseError, many, many1, digit, eof, choice)

import Control.Monad.Freer (Eff, Members, run, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)
import Control.Monad.Freer.State (State(..), get, gets, put, runState)
import Control.Monad.Freer.Writer (Writer(..), tell, runWriter)

--data Axiom =
--  CommuteSum |
--  AssociateSumLR |
--  AssociateSumRL |
--  CombineConst

data Axiom = Axiom {
  description :: String,
  example :: (Term, Term),
  implementation :: Term -> Either Term Term
}

instance Eq Axiom where
  a == b = description a == description b

instance Show Axiom where
  show (Axiom { description = d }) = d

axiomCommuteSum = Axiom {
  description = "Commutative law for addition",
  example = (Sum (Var "a") (Var "b"), Sum (Var "b") (Var "a")),
  implementation = f
}
  where
    f (Sum a b) = Right (Sum b a)
    f t = Left t

axiomAssociateSum = Axiom {
  description = "Associative law for addition",
  example = (
    Sum (Var "a") (Sum (Var "b") (Var "c")),
    Sum (Sum (Var "a") (Var "b")) (Var "c")
  ),
  implementation = f
}
  where
    f (Sum (Sum a b) c) = Right (Sum a (Sum b c))
    f (Sum a (Sum b c)) = Right (Sum (Sum a b) c)
    f t = Left t

axiomCommuteProduct = Axiom {
  description = "Commutative law for multiplication",
  example = (Product (Var "a") (Var "b"), Product (Var "b") (Var "a")),
  implementation = f
}
  where
    f (Product a b) = Right (Product b a)
    f t = Left t

axiomAssociateProduct = Axiom {
  description = "Associative law for multiplication",
  example = (
    Product (Var "a") (Product (Var "b") (Var "c")),
    Product (Product (Var "a") (Var "b")) (Var "c")
  ),
  implementation = f
}
  where
    f (Product (Product a b) c) = Right (Product a (Product b c))
    f (Product a (Product b c)) = Right (Product (Product a b) c)
    f t = Left t

axiomSumConst = Axiom {
  description = "Sum constants",
  example = (
    Sum (Const 1) (Const 2),
    (Const 3)
  ),
  implementation = f
}
  where
    f (Sum (Const a) (Const b)) = Right (Const $ a + b)
    f t = Left t

axiomMultiplyConst = Axiom {
  description = "Multiply constants",
  example = (
    Product (Const 2) (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (Product (Const a) (Const b)) = Right (Const $ a * b)
    f (Exponent (Const a) (Const b)) = Right (Const $ a ^ b)
    f t = Left t

axiomFactorialConst = Axiom {
  description = "Factorial constants",
  example = (
    Factorial (Const 3),
    (Const 6)
  ),
  implementation = f
}
  where
    f (Factorial (Const x)) = Right . Const $ factorial x
    f t = Left t

factorial 0 = 1
factorial x = x * factorial (x-1)

axiomIdentitySum = Axiom {
  description = "Additive identity",
  example = (
    (Sum (Sum (Const 0) (Var "a")) (Const 0)),
    (Var "a")
  ),
  implementation = f
}
  where
    f (Sum (Const 0) t) = Right t
    f (Sum t (Const 0)) = Right t
    f t = Left t

axiomIdentityProduct = Axiom {
  description = "Multiplicative identity",
  example = (
    (Product (Product (Const 1) (Var "a")) (Const 1)),
    (Var "a")
  ),
  implementation = f
}
  where
    f (Product (Const 1) t) = Right t
    f (Product t (Const 1)) = Right t
    f (Fraction t (Const 1)) = Right t
    f t = Left t

axiomNullExponent = Axiom {
  description = "Exponent of 0",
  example = (
    (Exponent (Var "a") (Const 0)),
    (Const 1)
  ),
  implementation = f
}
  where
    f (Exponent t (Const 0)) = Right (Const 1)
    f t = Left t

axiomIdentity = Axiom {
  description = "Identity",
  example = (
    (Var "a"),
    (Var "a")
  ),
  implementation = f
}
  where
    f t = Right t

axiomStepSeries = Axiom {
  description = "Step series",
  example = (
    (Series "k" (Var "a") (Var "k")),
    (Sum (Var "a") (Series "k" (Sum (Var "a") (Const 1)) (Var "k")))
  ),
  implementation = f
}
  where
    f (Series v i t) = Right $
      Sum
        (walk (instantiateVariable v i) t)
        (Series v (Sum i (Const 1)) t)
    f t = Left t

axiomDistribute = Axiom {
  description = "Distributive law",
  example = (
    (Product (Var "a") (Sum (Var "b") (Var "c"))),
    (Sum (Product (Var "a") (Var "b")) (Product (Var "a") (Var "c")))
  ),
  implementation = f
}
  where
    f (Sum (Product p1l p1r) (Product p2l p2r))
      | p1l == p2l = Right $ Product p1l (Sum p1r p2r)
    f (Product pl (Sum sl sr)) = Right $ Sum (Product pl sl) (Product pl sr)
    f (Sum v@(Var{}) p@(Product{})) = f (Sum (Product v (Const 1)) p)
    f t = Left t

instantiateVariable :: String -> Term -> Term -> Term
instantiateVariable name value (Var vname) | name == vname = value
instantiateVariable _ _ t = t

allAxioms =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomCommuteProduct
  , axiomAssociateProduct
  , axiomIdentitySum
  , axiomIdentityProduct
  , axiomNullExponent
  , axiomDistribute
  , axiomStepSeries
  , axiomSumConst
  , axiomMultiplyConst
  , axiomFactorialConst
  ]

data Term =
  Const Integer |
  Sum Term Term |
  Product Term Term |
  Var String |
  Series String Term Term |
  Factorial Term |
  Fraction Term Term |
  Exponent Term Term
  deriving (Show, Eq)

walk :: (Term -> Term) -> Term -> Term
walk f (Sum a b) = f (Sum (walk f a) (walk f b))
walk f (Product a b) = f (Product (walk f a) (walk f b))
walk f (Series v i t) = f (Series v (walk f i) (walk f t))
walk f (Factorial t) = f (Factorial (walk f t))
walk f (Fraction a b) = f (Fraction (walk f a) (walk f b))
walk f (Exponent a b) = f (Exponent (walk f a) (walk f b))
walk f t@(Const{}) = f t
walk f t@(Var{}) = f t

data Matcher =
  RootMatcher
  | LeftMatcher Term
  | AllMatcher
  | SeriesMatcher String

walkMatched :: Matcher -> (Term -> Either Term Term) -> Term -> Either Term Term
walkMatched m f t = Right $ walk f' t
  where
    f' :: Term -> Term
    f' t = if matcherApplies m t then
             case f t of
               Right t' -> t'
               Left t' -> t'
           else
             t

matcherApplies :: Matcher -> Term -> Bool
matcherApplies (LeftMatcher x) (Sum a _) = x == a
matcherApplies (LeftMatcher x) (Product a _) = x == a
matcherApplies (SeriesMatcher v) (Series v' _ _) = v == v'
matcherApplies AllMatcher _ = True
matcherApplies _ _ = False

--modify :: Matcher -> (Term -> Either Term Term) -> Term -> Either Term Term
--modify (LeftMatcher x) f t@(Sum a b) | x == a = f t
--modify (LeftMatcher x) f t@(Product a b) | x == a = f t
--modify RootMatcher f t = f t
--modify (SeriesMatcher v1) f (Series v2 i t) | v1 == v2 = (modify (SeriesMatcher v1) f t) >>= \inner -> f (Series v2 i inner)
--modify m f (Sum a b) =
--  let
--    rhs = modify m f a
--    lhs = modify m f b
--  in
--    Sum <$> rhs <*> lhs
--modify m f (Product a b) =
--  let
--    rhs = modify m f a
--    lhs = modify m f b
--  in
--    Product <$> rhs <*> lhs
--modify m f (Const a) = Right (Const a)
--modify m f t = Right t

toAscii (Const a) = show a
toAscii (Var a) = a
toAscii (Sum a b) = "(" <> toAscii a <> " + " <> toAscii b <> ")"
toAscii (Product a b) = "(" <> toAscii a <> "⋅" <> toAscii b <> ")"
toAscii (Series v i t) = "Σ[" <> v <> " = " <> toAscii i <> "](" <> toAscii t <> ")"
toAscii (Factorial t) = "(" <> toAscii t <> ")!"
toAscii (Fraction a b) = "(" <> toAscii a <> "/" <> toAscii b <> ")"
toAscii (Exponent a b) = toAscii a <> "^" <> toAscii b

data Env = Env Term deriving (Show)

type Log = [(Term, Axiom)]
type AppEff effs = Members '[ Writer Log, State Env ] effs

ignoreError :: Either a a -> a
ignoreError (Left x) = x
ignoreError (Right x) = x

apply :: AppEff effs => Matcher -> Axiom -> Eff effs ()
apply m axiom = do
  (Env t) <- get

  -- TODO: Handle error
  case walkMatched m (implementation axiom) t of
    Right t' -> do
      let t'' = walk (
                  ignoreError . implementation axiomSumConst .
                  ignoreError . implementation axiomMultiplyConst .
                  ignoreError . implementation axiomNullExponent
                ) t'
      tell [(t'', axiom)]

      put (Env t'')
    Left t' -> do
      error $ "couldn't apply " <> description axiom <> " to " <> toAscii t' <> " (full term is " <> toAscii t <> ")"

--body = runProcess (Sum (Var "x") (Sum (Const 2) (Const 3))) $ do
--  apply RootMatcher axiomCommuteSum
--  apply RootMatcher axiomAssociateSum
--  apply RootMatcher axiomAssociateSum
--  apply (LeftMatcher (Const 2)) axiomSumConst

--body = runProcess (Series "k" (Const 0) (Sum (Var "x") (Var "k"))) $ do
--  apply (SeriesMatcher "k") axiomStepSeries
--  apply (SeriesMatcher "k") axiomStepSeries

--body = runProcess (Product (Const 2) (Product (Const 3) (Const 4))) $ do
--  apply RootMatcher axiomCommuteProduct
--  apply (LeftMatcher (Const 3)) axiomMultiplyConst

--body = runProcess (Sum (Var "x") (Product (Var "x") (Var "x"))) $ do
--  apply RootMatcher axiomDistribute
--  apply RootMatcher axiomDistribute
  --apply RootMatcher axiomDistribute

-- TODO: Handle variable aliasing properly for nested series
e_to t = (Series "k" (Const 0) (Fraction (Exponent t (Var "k")) (Factorial (Var "k"))))
cos_x = (Series "m" (Const 0) (Product (Exponent (Const (-1)) (Var "m")) (Fraction (Exponent (Var "x") (Product (Const 2) (Var "m"))) (Factorial (Product (Const 2) (Var "m"))))))

body = runProcess (e_to cos_x) $ do
  apply (SeriesMatcher "m") axiomStepSeries
  apply (SeriesMatcher "k") axiomStepSeries
  apply AllMatcher axiomFactorialConst
  apply AllMatcher axiomIdentityProduct
  apply (SeriesMatcher "m") axiomStepSeries
  apply (SeriesMatcher "k") axiomStepSeries
  apply AllMatcher axiomFactorialConst
  apply AllMatcher axiomIdentityProduct
  --apply RootMatcher axiomAssociateSum

printAxioms axioms = do
  let paddingIndex = length (show $ length axioms)
  let paddingDesc = maximum . map (length . description) $ axioms
  forM_ (zip axioms [1..]) $ \(axiom, i) -> do
    putStr (show i)
    putStr ". "
    putStr $ replicate (paddingIndex - length (show i)) ' '
    putStr (description axiom)
    putStr ": "
    putStr $ replicate (paddingDesc - length (description axiom)) ' '
    let (lhs, rhs) = example axiom
    putStr $ toAscii lhs
    putStr " = "
    putStrLn $ toAscii rhs

runProcess t m = do
  let (_, log) = runApp (Env t) m
  let usedAxioms = nub (map snd log)
  printAxioms usedAxioms
  putStrLn ""

  let paddingT = maximum $ map (length . toAscii . fst) log
  putStrLn . toAscii $ t
  forM_ log $ \(t, axiom) -> do
    putStr (toAscii t)
    putStr $ replicate (paddingT - length (toAscii t)) ' '
    putStrLn $ " ; " <> description axiom

main = body

runApp :: Env -> Eff '[ Writer Log, State Env] a -> (Term, Log)
runApp env m = do
  let ((_, log), (Env t)) = run . runState env . runWriter $ m

  (t, log)
