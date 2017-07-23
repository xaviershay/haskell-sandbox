{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Types
import Parser
import Evaluator
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.HashSet           as S
import qualified Data.Text              as T
import qualified Data.Vector            as V

relation :: [Identifier] -> [[Identifier]] -> Relation
relation hs es = Relation { headers = V.fromList hs, elements = S.fromList (map V.fromList es) }

testData = putEnv "person" (relation ["name", "age"] [["alice", "12"], ["bob", "13"]]) emptyEnv

doEval env query = parseSql query >>= runEval testData

main :: IO ()
main =
  defaultMain $
    testGroup "SELECT"
      [ testCase "Test a thing" $
          (Right $ relation ["name"] [["alice"], ["bob"]]) @=?
            doEval testData "SELECT name FROM person"
      ]
