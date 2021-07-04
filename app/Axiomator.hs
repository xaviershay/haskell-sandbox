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

axiomSumConst = Axiom {
  description = "Sum constants",
  example = (
    Sum (Var "a") (Const 2),
    (Var "c")
  ),
  implementation = f
}
  where
    f (Sum (Const a) (Const b)) = Right (Const $ a + b)
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

instantiateVariable :: String -> Term -> Term -> Term
instantiateVariable name value (Var vname) | name == vname = value
instantiateVariable _ _ t = t

allAxioms =
  [ axiomCommuteSum
  , axiomAssociateSum
  , axiomSumConst
  , axiomIdentity
  , axiomStepSeries
  ]

data Term =
  Const Integer |
  Sum Term Term |
  Var String |
  Series String Term Term
  deriving (Show, Eq)

walk :: (Term -> Term) -> Term -> Term
walk f (Sum a b) = f (Sum (walk f a) (walk f b))
walk f (Series v i t) = f (Series v (walk f i) (walk f t))
walk f t@(Const{}) = f t
walk f t@(Var{}) = f t

data Matcher =
  RootMatcher
  | LeftMatcher Term
  | SeriesMatcher

modify :: Matcher -> (Term -> Either Term Term) -> Term -> Either Term Term
modify (LeftMatcher x) f t@(Sum a b) | x == a = f t
modify RootMatcher f t = f t
modify SeriesMatcher f t@(Series{}) = f t
modify m f (Sum a b) =
  let
    rhs = modify m f a
    lhs = modify m f b
  in
    Sum <$> rhs <*> lhs
modify m f (Const a) = Right (Const a)
modify m f t = Right t

toAscii (Const a) = show a
toAscii (Var a) = a
toAscii (Sum a b) = "(" <> toAscii a <> " + " <> toAscii b <> ")"
toAscii (Series v i t) = "Σ[" <> v <> " = " <> toAscii i <> "](" <> toAscii t <> ")"

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
  case modify m (implementation axiom) t of
    Right t' -> do
      let t'' = walk (ignoreError . implementation axiomSumConst) t'
      tell [(t'', axiom)]

      put (Env t'')
    Left t' -> do
      error $ "couldn't apply " <> description axiom <> " to " <> toAscii t' <> " (full term is " <> toAscii t <> ")"


runProcess t m = do
  putStrLn . toAscii $ t
  let (_, log) = runApp (Env t) m
  forM_ log $ \(t, axiom) -> do
    putStr (toAscii t)
    putStrLn $ "\t; " <> description axiom

--body = runProcess (Sum (Var "x") (Sum (Const 2) (Const 3))) $ do
--  apply RootMatcher axiomCommuteSum
--  apply RootMatcher axiomAssociateSum
--  apply RootMatcher axiomAssociateSum
--  apply (LeftMatcher (Const 2)) axiomSumConst

body = runProcess (Series "k" (Const 0) (Sum (Var "x") (Var "k"))) $ do
  apply SeriesMatcher axiomStepSeries
  apply SeriesMatcher axiomStepSeries

main = do
  forM_ (zip allAxioms [1..]) $ \(axiom, i) -> do
    putStr (show i)
    putStr ". "
    putStr (description axiom)
    putStr ":\t"
    let (lhs, rhs) = example axiom
    putStr $ toAscii lhs
    putStr " = "
    putStrLn $ toAscii rhs

  putStrLn ""

  body

runApp :: Env -> Eff '[ Writer Log, State Env] a -> (Term, Log)
runApp env m = do
  let ((_, log), (Env t)) = run . runState env . runWriter $ m

  (t, log)
