module Evaluator where

import qualified Data.HashSet           as S
import Data.Witherable (catMaybes)

import Control.Monad.Reader (ask, runReader)

import qualified Data.Vector as V

import Types

runEval env q = runReader (eval q) env

eval :: Expression -> Eval Relation
eval (RelationFromEnv name) = do
  env <- ask
  return env

eval (Project columns x) = do
  r@Relation { headers = prevHeaders, elements = prevElements } <- eval x

  let indices = catMaybes $ V.map (\x -> V.elemIndex x prevHeaders) columns
  -- TODO: Use safe lookup
  let f = \row -> V.map (\i -> row V.! i) indices

  return $ Relation {
    headers = columns,
    elements = S.map f prevElements
  }

