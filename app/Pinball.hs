{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics

data Transition a b = Transition
  { _transitionLabel :: String
  , _transitionF :: Machine a b -> Machine a b
  }

instance Eq (Transition a b) where
  x == y = _transitionLabel x == _transitionLabel y

instance Show (Transition a b) where
  show x = _transitionLabel x

data Machine a b = Machine
  { _machineTransitions :: M.HashMap a (Transition a b)
  , _machineCounters :: M.HashMap b Int
  } deriving (Show, Eq)

makeLenses ''Transition

makeLenses ''Machine

data Target
  = TargetLeftRamp
  | TargetRight
  | TargetAcMag
  | TargetComputer
  | TargetLeftLoop
  deriving (Show, Eq, Generic)

data Counter
  = CountScore
  | CountCombos
  deriving (Show, Eq, Generic)

instance Hashable Target

instance Hashable Counter

type DemoMan = Machine Target Counter

initialMachine :: DemoMan
initialMachine =
  Machine {_machineTransitions = mempty, _machineCounters = mempty}

mkTransition label f = Transition {_transitionLabel = label, _transitionF = f}

identityTransition = mkTransition "identity" id

applyTransition t m = (_transitionF t) m

setTransitions list m = m {_machineTransitions = M.fromList list}

incrementCounter name amount =
  over (machineCounters . at name . non 0) (+ amount)

shoot :: (Hashable a, Eq a) => a -> Machine a b -> Machine a b
shoot target machine =
  applyTransition
    (M.lookupDefault identityTransition target (_machineTransitions machine))
    machine


resetCombo :: Transition Target Counter
resetCombo =
  mkTransition
    "reset combo"
    (setTransitions
       [ (TargetLeftRamp, continueCombo TargetRight 5000000)
       , (TargetRight, resetCombo)
       ])

continueCombo :: Target -> Int -> Transition Target Counter
continueCombo nextShot points =
  mkTransition
    "continue combo"
    (setTransitions
       [ (nextShot, continueCombo nextNextShot $ points + 1000000)
       , (nextNextShot, resetCombo)
       ] .
     incrementCounter CountScore points)
  where
    nextNextShot =
      case nextShot of
        TargetLeftRamp -> TargetRight
        TargetRight -> TargetLeftRamp

data QuestCondition =
    CondHitTarget Target
  | CondAny [QuestCondition]
  | CondAll [QuestCondition]
  | CondNone
  deriving (Eq)

data Reward = RewardPoints Int | RewardMulti Int
  deriving (Show)

data Quest a = Quest
  { _questCondition :: QuestCondition
  , _questLabel :: String
  , _questSuccess :: Quest a -> (Quest a, Reward)
  , _questState :: a
  }
makeLenses ''Quest

shoot2 :: Target -> Int -> Int
shoot2 = undefined

-- makeQuest "Fortress Multiball"

emptyQuest :: Quest String
emptyQuest = makeSimpleQuest "" CondNone (RewardPoints 0)

makeSimpleQuest label cond reward = Quest
  { _questCondition = cond
  , _questLabel = label
  , _questSuccess = \_ -> (emptyQuest, reward)
  , _questState = mempty
  }

isComplete quest = view questCondition quest == CondNone

sequenceQuests (q:qs) | isComplete q = sequenceQuests qs
sequenceQuests (q:qs) =
  over questSuccess (\f -> \a -> let (next, reward) = f a in (sequenceQuests (next:qs), reward)) q
sequenceQuests [] = emptyQuest

fortress =
  sequenceQuests
    [ makeSimpleQuest "AcMag" (CondHitTarget TargetAcMag) (RewardPoints 25)
    , makeSimpleQuest "Computer" (CondHitTarget TargetComputer) (RewardPoints 26)
    , fortress
    ]

questApplies :: Target -> Quest a -> Bool
questApplies target quest =
  case view questCondition quest of
    CondHitTarget target2 -> target == target2
    _ -> False

hit :: Target -> Quest a -> IO (Quest a)
hit target quest =
  if questApplies target quest then
    do
      let (next, reward) = (view questSuccess quest) quest
      putStrLn . show $ reward
      return next
  else
    return quest

main = do
  let q = fortress
  q <- hit TargetAcMag q
  q <- hit TargetComputer q
  q <- hit TargetAcMag q

  return ()
