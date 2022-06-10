{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Pinball where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics
import Control.Lens

data Transition a b = Transition
  { _transitionLabel :: String
  , _transitionF :: Machine a b -> Machine a b
  }

instance Eq (Transition a b) where
  x == y = _transitionLabel x == _transitionLabel y

instance Show (Transition a b) where
  show x = _transitionLabel x

data Machine a b = Machine {
    _machineTransitions :: M.HashMap a (Transition a b)
  , _machineCounters :: M.HashMap b Int
} deriving (Show, Eq)

makeLenses ''Transition
makeLenses ''Machine

data Target = TargetLeft | TargetRight
  deriving (Show, Eq, Generic)
data Counter = CountScore | CountCombos
  deriving (Show, Eq, Generic)

instance Hashable Target
instance Hashable Counter

type DemoMan = Machine Target Counter

initialMachine :: DemoMan
initialMachine = Machine {
    _machineTransitions = mempty
  , _machineCounters = mempty
}

mkTransition label f = Transition
  { _transitionLabel = label
  , _transitionF = f
  }
identityTransition = mkTransition "identity" id
 
applyTransition t m = (_transitionF t) m

setTransitions list m = m { _machineTransitions = M.fromList list }
incrementCounter name amount = over (machineCounters . at name . non 0) (+ amount)

shoot :: (Hashable a, Eq a) => a -> Machine a b -> Machine a b
shoot target machine =
  applyTransition (M.lookupDefault identityTransition target (_machineTransitions machine)) machine

main = putStrLn . show $ shoot TargetRight . shoot TargetRight . (shoot TargetLeft) $ (applyTransition resetCombo) initialMachine

resetCombo :: Transition Target Counter
resetCombo = mkTransition "reset combo" (setTransitions [(TargetLeft, continueCombo TargetRight 5_000_000), (TargetRight, resetCombo)])

continueCombo :: Target -> Int -> Transition Target Counter
continueCombo nextShot points = mkTransition "continue combo"
  (setTransitions [(nextShot, continueCombo nextNextShot $ points + 1_000_000), (nextNextShot, resetCombo)] .  incrementCounter CountScore points)

  where
    nextNextShot = case nextShot of
                     TargetLeft -> TargetRight
                     TargetRight -> TargetLeft
