{-# LANGUAGE OverloadedStrings #-}
module Evaluator where

import qualified Data.HashSet           as S
import Data.Witherable (catMaybes)

import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Except
import Control.Monad.Identity

import qualified Data.Vector as V

import Types

type EvalResult = Either EvalError Relation

runEval :: Env -> Expression -> EvalResult
runEval env q = runIdentity $ runReaderT (runExceptT (eval q)) env

env :: Eval Env
env = ask

failWith :: EvalError -> Maybe a -> Eval a
failWith err = maybe (throwError err) return

eval :: Expression -> Eval Relation
eval (RelationFromEnv name) =
  env >>= failWith (RelationNotFound name) . relationFromEnv name

eval (Project columns x) = do
  r@Relation { headers = prevHeaders, elements = prevElements } <- eval x

  let indices = catMaybes $ V.map (`V.elemIndex` prevHeaders) columns
  -- TODO: Use safe lookup
  let f row = V.map (\i -> row V.! i) indices

  return Relation {
    headers = columns,
    elements = S.map f prevElements
  }

