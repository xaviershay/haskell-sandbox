{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Control.Monad

import qualified Data.Heap as H

data UnitTask = GatheringMinerals | GatheringGas deriving (Show)
data UnitState = Idle | Active UnitTask Float deriving (Show)

data Unit = Zergling | Queen deriving (Show)

data GameState = GameState
  { _units :: [(Unit, UnitState)]
  , _actions :: H.MinPrioHeap Float GameAction
  , _collectingMinerals :: Int
  , _collectingGas :: Int
  , _minerals :: Int
  , _gas :: Int
  , _time :: Float
  } deriving (Show)

miningTime = 7.5 :: Float

initialState = GameState
  { _units = mempty
  , _actions = H.fromList [(miningTime / 12, CollectMinerals)]
  , _collectingMinerals = 12
  , _collectingGas = 3
  , _minerals = 50
  , _gas = 0
  , _time = 0
  }

data GameAction = CollectMinerals | CollectGas | SpawnCollectMinerals | SpawnLarvae | Inject deriving (Show)

makeLenses ''GameState

type Score = Float

score :: GameState -> Score
score state = view time state

finishTime :: GameState -> Maybe Float
finishTime state =
  if view minerals state < 300 then
    Nothing
  else
    Just $ view time state

testFormat state =
  show (view time state) <> ": " <> show (view minerals state) <> " " <> show (view collectingMinerals state)

main = do
  let best = initialState
  let states = H.fromList [(score best, best)] :: H.MinPrioHeap Score GameState
  
  go states

  where
    go :: H.MinPrioHeap Score GameState -> IO ()
    go states = do
      case H.view states of
        Nothing -> do
          putStrLn $ "No more states."
        Just ((n, state), rest) -> do
          --putStrLn ""
          putStrLn $ "---- Evaluating: " <> show n <> ": " <> show (H.size states)
          --putStrLn $ show state
          --forM_ rest $ \r -> do
          --  putStrLn $ show r

          case finishTime state of
            Just t -> do
              putStrLn "Done!"
              putStrLn $ show state
            Nothing -> do
              let newStates = map step (choose state) :: [GameState]
              let scored = map (\x -> (score x, x)) newStates

              go (H.union (H.fromList scored) rest)

          --forM_ (H.toList states) $ \(p, s) -> do
          --  putStrLn $ show s


choose :: GameState -> [GameState]
choose state = [state] <>
  if view minerals state >= 50 then
    [over minerals (\x -> x - 50)
      . over actions (H.insert (view time state + 12, SpawnCollectMinerals))
      $ state]
  else
    []

step :: GameState -> GameState
step state =
  let Just ((t, a), as) = H.view (_actions state) in

  stepAction (set time t . set actions as $ state) a

stepAction :: GameState -> GameAction -> GameState
stepAction state SpawnCollectMinerals =
  over collectingMinerals (+ 1) state

stepAction state CollectMinerals =
  let newT = view time state + (miningTime / (fromIntegral $ view collectingMinerals state)) in

  over minerals (\x -> x + 5)
    . over actions (H.insert (newT, CollectMinerals))
    $ state