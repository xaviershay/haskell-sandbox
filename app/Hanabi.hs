{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- Experimenting with modelling the induction logic needed in the card game
-- Hanabi. This implements "static" inductions - things that can be inferred
-- from known information (things told by other players and the current state
-- of the game), which are quite simple. It uses an iterative constraint
-- solver: apply all the known constraints for every unknown card to filter
-- down the set of guesses, then repeat until the set of guesses doesn't
-- change.
--
-- These are admittedly the least interesting inductions in Hanabi. Two more
-- interesting ones are probabalistic inductions which rely on assumptions
-- about other people's (unpredictable!) behaviour and on the flow of the game.
--
-- * Intent induction. If a 1 is on the table and someone tells me I
--   have a 2, I should probably induce that the color of the 2 matches the 1.
--   Otherwise, why would they tell me?
-- * Temporal induction. If people have had the opportunity to tell me things
--   about my cards but haven't, that changes my probability assessments when
--   choosing to discard/play. For example, if I have a 5 and am risk of
--   discarding, someone should probably have told me. Therefore I likely don't
--   have a 5.
module Main where

import Control.Lens
import Control.Monad
import Data.Function (on)
import qualified Data.HashMap.Strict as M
import Data.List ((\\), delete, nub)
import Data.Maybe (catMaybes)
import Debug.Trace
import GHC.Generics

data Color =
    Red
  | Green
  | Blue
  | Yellow
  | White
  deriving (Show, Enum, Bounded, Generic, Eq)

type Rank = (Color, Int)

frequencies =
  [ (1, 3)
  , (2, 2)
  , (3, 2)
  , (4, 2)
  , (5, 1)
  ]

allColors :: [Color]
allColors = [minBound..maxBound]
allCards = concat [replicate f (c, n) | c <- allColors, (n, f) <- frequencies]

data State = State
  { _hands :: [[Rank]]
  , _deck :: [Rank]
  , _table :: M.HashMap Color Int
  , _discard :: [Rank]
  , _mine :: [UnknownCard]
  } deriving (Show)

data UnknownCard = UnknownCard
  { _known :: Info
  , _guesses :: [Rank]
  , _actual :: Rank
  } deriving (Show)

-- Law: the returned list from filtering function must have length <= to input
-- list
data Info = Info (String, State -> [Rank] -> [Rank])

instance Show Info where
  show (Info (l, _)) = l

instance Semigroup Info where
  Info (l1, f1) <> Info (l2, f2) = Info (l1 <> " " <> l2, newF)
    where
      newF state rs = f2 state (f1 state rs)

instance Monoid Info where
  mempty = Info ("", \_ cs -> cs)

makeLenses ''State
makeLenses ''UnknownCard

initialState = State
  { _hands = mempty
  , _deck = allCards
  , _table = M.empty
  , _discard = mempty
  , _mine = mempty
  }

drawMine :: State -> State
drawMine state =
  let (d:dd) = view deck state in
  state { _mine = (mkUnknown d:_mine state), _deck = dd }


mkUnknown rank = UnknownCard
  { _known = mempty
  , _guesses = nub allCards
  , _actual = rank
  }

tellColor :: Color -> Info
tellColor knownColor = Info ("Color = " <> show knownColor, f)
  where
    f _ = filter ((==) knownColor . fst)

tellN :: Int -> Info
tellN known = Info ("N = " <> show known, f)
  where
    f _ = filter ((==) known . snd)

excludeVisible :: Info
excludeVisible = Info ("Exclude visible", f)
  where
    f state rs =
      -- TODO: Include table
      let visible =
              concat (view hands state)
            <> view discard state
            <> catMaybes (map knownFromMine $ view mine state)
      in

      let ranksToInclude = nub (allCards \\ visible) in

      filter (`elem` ranksToInclude) rs
    knownFromMine :: UnknownCard -> Maybe Rank
    knownFromMine x =
      case view guesses x of
        [r] -> Just r
        _   -> Nothing

infer :: State -> UnknownCard -> UnknownCard
infer state unknown =
  let Info (_, f) = excludeVisible <> view known unknown in

  over guesses (excludeKnown $ f state) unknown

  where
    -- Don't run inference on a card that is already known, since some of the
    -- algorithms would use this information to then incorrectly exclude it.
    excludeKnown :: ([Rank] -> [Rank]) -> [Rank] -> [Rank]
    excludeKnown f [c] = [c]
    excludeKnown f cs = f cs

placeInDiscard rank state =
  over deck (delete rank)
  . over discard (rank:)
  $ state

-- For each unknown card, infer it. Repeat until stable
solve :: State -> State
solve state =
  let
    oldMine = view mine state
    newMine = map (infer state) oldMine
    newState = set mine newMine state
  in

  if ((==) `on` map (view guesses)) oldMine newMine then
    newState
  else
    solve newState

main = do
  let state =
        over (mine . ix 0 . known) (tellColor Red <>)
          . over (mine . ix 1 . known) (tellColor Red <>)
          . over (mine . ix 1 . known) (tellN 4 <>)
          . placeInDiscard (Red, 5)
          . placeInDiscard (Red, 4)
          . drawMine
          . drawMine
          $ initialState
  putStrLn . show $ solve state
