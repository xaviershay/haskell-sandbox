{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Hashable
import GHC.Generics hiding (Selector)
import qualified Data.HashMap.Strict as M
import Control.Arrow ((>>>))

import Test.Tasty
import Test.Tasty.HUnit

type Signals = M.HashMap String Int
type Combinator = Signals -> Signals

signals = M.fromList

mathAll :: (Int -> Int) -> Combinator
mathAll f inputs = fmap f inputs

mathSignal :: (Int -> Int -> Int) -> (Signals -> Int) -> Combinator
mathSignal f yF inputs =
  mathAll (f $ yF inputs) inputs

signalValue :: String -> Signals -> Int
signalValue = M.lookupDefault 0

data Selector = All | Each | Any | Single String | Const Int

boolean :: Selector -> (Int -> Int -> Int) -> (Signals -> Int) -> Selector -> (Signals -> Signals)
boolean Any op rhsF (Single outputSignal) input =
  let y = rhsF input in
  if any (\v -> op v y > 0) input then
    signals [(outputSignal, 1)]
  else
    mempty

boolean All op rhsF (Single outputSignal) input =
  let y = rhsF input in
  if all (\v -> op v y > 0) input then
    signals [(outputSignal, 1)]
  else
    mempty

boolean Each op rhsF Each input =
  let y = rhsF input in

  M.filter (\v -> op v y > 0) input

boolean (Single inputSignal) op rhsF (Single outputSignal) input =
  let rhs = rhsF input in
  let lhs = (signalValue inputSignal) input in
  if (op lhs rhs) > 0 then
    signals [(outputSignal, 1)]
  else
    mempty

math :: Selector -> (Int -> Int -> Int) -> (Signals -> Int) -> (Signals -> Signals)
math All op rhsF input =
  let rhs = rhsF input in
  M.map ((flip op) rhs) input

math (Single s) op rhsF input =
  let rhs = rhsF input in
  signals [(s, op rhs (signalValue s input))]

lte :: Int -> Int -> Int
lte x y = if x <= y then 1 else 0

gte :: Int -> Int -> Int
gte x y = if x >= y then 1 else 0

add :: Signals -> Signals -> Signals
add = M.unionWith (+)

launch :: Signals -> Signals -> Signals -> Bool
launch requested loaded remote =
  let
    brownoutConstant = signals [("black", -1)]

    whatDoWeWant = math All (*) (const (-1)) $ requested
    brownoutFakeRequests =
        math All (*) (signalValue "black") $
        (math (Single "black") (*) (const (-1000000)))
        (remote `add` brownoutConstant) `add` whatDoWeWant

    filterable =
        boolean Each gte (const 1000000) Each $
        ((math All (*) (const (-1))
           >>> math All (+) (const (1000000)) $ requested)
        `add` remote)
    filterRequested =
           ((math All (-) (const 1000000)) $ filterable) `add` requested

    differential = 
        math All (*) (const (-1)) >>> boolean Each gte (const 0) Each $ remote `add` requested `add` loaded `add` brownoutFakeRequests

    condNoBrownout = boolean (Single "black") gte (const 0) (Single "yellow") $ remote `add` brownoutConstant
    condDestEmpty = boolean Any lte (const 2) (Single "yellow") $ filterRequested
    condSrcFull = boolean All lte (const 0) (Single "yellow") $ differential

    launcher = boolean (Single "yellow") gte (const 3) (Single "green")
                   $ (condNoBrownout `add` condDestEmpty `add` condSrcFull)
  in

    M.lookupDefault 0 "green" launcher > 0
  
main = do
  let requested = signals [("repair", -50), ("bot", -50)]
  let loaded = signals [("repair", 10), ("bot", 50)]
  let remote = if True then signals [("black", 1), ("bot", 0), ("repair", 49)] else mempty

  let brownoutConstant = signals [("black", -1)]

  let whatDoWeWant = math All (*) (const (-1)) $ requested
  let brownoutFakeRequests =
        math All (*) (signalValue "black") $
        (math (Single "black") (*) (const (-1000000)))
        (remote `add` brownoutConstant) `add` whatDoWeWant

  let filterable =
        boolean Each gte (const 1000000) Each $
        ((math All (*) (const (-1))
           >>> math All (+) (const (1000000)) $ requested)
        `add` remote)
  let filterRequested =
           ((math All (-) (const 1000000)) $ filterable) `add` requested

  let differential = 
        math All (*) (const (-1)) >>> boolean Each gte (const 0) Each $ remote `add` requested `add` loaded `add` brownoutFakeRequests

  let condNoBrownout = boolean (Single "black") gte (const 0) (Single "yellow") $ remote `add` brownoutConstant
  let condDestEmpty = boolean Any lte (const 2) (Single "yellow") $ filterRequested
  let condSrcFull = boolean All lte (const 0) (Single "yellow") $ differential

  let launcher = boolean (Single "yellow") gte (const 3) (Single "green")
                   $ (condNoBrownout `add` condDestEmpty `add` condSrcFull)

  --putStrLn . show . network $ signals [("blue", 2), ("red", (-1))]
  putStrLn "These items are what is requested"
  putStrLn . show $ requested
  putStrLn ""

  putStrLn "These items are present at the destination"
  putStrLn . show $ filterRequested
  putStrLn ""

  putStrLn "These items are loaded"
  putStrLn . show $ loaded
  putStrLn ""

  putStrLn "These items still need to be loaded"
  putStrLn . show $ differential
  putStrLn ""


  --putStrLn . show $ brownoutFakeRequests
  --putStrLn . show $ filterable

  putStr "No brownout: "
  putStrLn . show $ condNoBrownout

  putStr "Dest empty:  "
  putStrLn . show $ condDestEmpty

  putStr "Full load:   "
  putStrLn . show $ condSrcFull

  putStr "Launch?:    "
  putStrLn . show $ launcher

  defaultMain tests

tests = testGroup "Launch" $
  [ testCase "Launches with full load when destination is empty" $
      True @=?
        launch
          (signals [("repair", -50)]) -- Requested
          (signals [("repair", 50)])  -- Loaded
          (signals [("black", 1)])    -- Remote
  , testCase "No launch with full load when destination is not empty" $
      False @=?
        launch
          (signals [("repair", -50)]) -- Requested
          (signals [("repair", 50)])  -- Loaded
          (signals [("black", 1), ("repair", 10)])    -- Remote
  , testCase "No launch with full load when brownout" $
      False @=?
        launch
          (signals [("repair", -50)]) -- Requested
          (signals [("repair", 50)])  -- Loaded
          (signals [])
  , testCase "No launch when not full" $
      False @=?
        launch
          (signals [("repair", -50)]) -- Requested
          (signals [("repair", 40)])  -- Loaded
          (signals [("black", 1)])    -- Remote
  ]

tests2 = testGroup "Combinators"
  [ testCase "boolean any all" $
      signals [("yellow", 1)] @=?
      boolean Any gte (const 10) (Single "yellow") (signals [("blue", 10)])
  , testCase "boolean any none" $
      signals [] @=?
      boolean Any gte (const 10) (Single "yellow") (signals [("blue", 9)])
  , testCase "boolean any at least one" $
      signals [("yellow", 1)] @=?
      boolean Any gte (const 10) (Single "yellow") (signals [("blue", 9), ("red", 11)])
  , testCase "boolean each" $
      signals [("red", 11)] @=?
      boolean Each gte (const 10) Each (signals [("blue", 9), ("red", 11)])
  , testCase "boolean all all" $
      signals [("yellow", 1)] @=?
      boolean All gte (const 10) (Single "yellow") (signals [("blue", 10)])
  , testCase "boolean all none" $
      signals [] @=?
      boolean All gte (const 10) (Single "yellow") (signals [("blue", 9)])
  , testCase "boolean all at least one" $
      signals [] @=?
      boolean All gte (const 10) (Single "yellow") (signals [("blue", 9), ("red", 11)])
  , testCase "boolean single all" $
      signals [("yellow", 1)] @=?
      boolean (Single "blue") gte (const 10) (Single "yellow") (signals [("blue", 10)])
  , testCase "boolean all none" $
      signals [] @=?
      boolean (Single "red") gte (const 10) (Single "yellow") (signals [("blue", 9)])
  , testCase "boolean all at least one matching" $
      signals [("yellow", 1)] @=?
      boolean (Single "red") gte (const 10) (Single "yellow") (signals [("blue", 9), ("red", 11)])
  , testCase "boolean all at least one not" $
      signals [] @=?
      boolean (Single "blue") gte (const 10) (Single "yellow") (signals [("blue", 9), ("red", 11)])
  , testCase "math all * by constant" $
      signals [("red", 4), ("blue", 6)] @=?
      math All (*) (const 2) (signals [("red", 2), ("blue", 3)])
  , testCase "math all * by signal" $
      signals [("red", 6), ("blue", 9)] @=?
      math All (*) (signalValue "blue") (signals [("red", 2), ("blue", 3)])
  ]
