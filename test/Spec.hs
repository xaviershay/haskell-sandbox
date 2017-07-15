{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

import Parser
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Data.HashSet           as S
import qualified Data.Text              as T

relation :: [Identifier] -> [Row] -> Relation
relation hs es = Relation { headers = hs, elements = S.fromList es }

testData = (relation ["name", "age"] [["alice", "12"], ["bob", "13"]])

--eval :: Relation -> Query -> Either T.Text Relation
eval env query = parseSql query

main :: IO ()
main = do
  defaultMain $
    testGroup "SELECT"
      [ testCase "Test a thing" $
          --(Right $ relation ["name"] [["alice"], ["bob"]]) @=?
          (Right $ Select ["name"] (RelationFromEnv "person")) @=?
            eval testData "SELECT name FROM person"
      ]
