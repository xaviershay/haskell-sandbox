-- Really terrible PoC code messing around with solving sudoku problems without dynamic programming

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))
import Data.List (foldl', (\\), subsequences)
import Data.Foldable (toList)
import qualified Data.Text as T
import Control.Monad (forM_, when)
import Data.Hashable
import GHC.Generics
import Debug.Trace
import Data.Sort (sortOn)
import Control.Monad.Freer (Eff, Members, run, runM)
import Control.Monad.Freer.Error (Error, throwError, runError)
import Control.Monad.Freer.State (State(..), get, gets, put, runState)
import Control.Lens (view, makeLenses, _Just, at)
import Control.Lens (ASetter, view, over, set)

data Group =
    AllValues (S.HashSet CellId) -- This set of cells must span all values
  | Sandwich Int [CellId]        -- The cells between min/max value must eq value
  deriving (Show)



instance Semigroup Group where
  -- See comment on SemiGroup Cell
  a <> b = error "don't use this"

instance Monoid Group where
  mempty = AllValues mempty

type Constraint = S.HashSet Int

data Problem = Problem
  { _problemCells :: M.HashMap CellId Cell
  , _problemStructure :: Structure
  }

type Structure = M.HashMap GroupId Group

-- =========
-- Reimplementations of Control.Lens MTL helpers to work with freer-simple
assign :: Members '[ State s ] effs => ASetter s s a b -> b -> Eff effs ()
assign accessor value = get >>= put . set accessor value

modifying :: Members '[ State s ] effs => ASetter s s a b -> (a -> b) -> Eff effs ()
modifying accessor f = get >>= put . over accessor f
-- =========

type AppEff effs = Members '[ State Problem ] effs

runApp :: Problem -> Eff '[ State Problem ] a -> Problem
runApp state m =
  let (val, problem) = run . runState state $ m in

  problem

newtype GroupId = GroupId T.Text deriving (Eq, Generic, Show)
newtype CellId = CellId T.Text deriving (Eq, Generic, Show)

instance Hashable CellId
instance Hashable GroupId

data Cell = Cell
  { _cellId :: CellId
  , _cellConstraint :: Constraint
  , _cellGroups :: S.HashSet GroupId
  } deriving (Show)

instance Eq Cell where
  Cell { _cellId = a } == Cell { _cellId = b } = a == b

instance Hashable Cell where
  hashWithSalt n (Cell {_cellId = a}) = hashWithSalt n a

makeLenses ''Cell
makeLenses ''Problem

allValues = [1..4]

mkCell :: T.Text -> [T.Text] -> Cell
mkCell name groupIds = Cell
  { _cellId = CellId name
  , _cellConstraint = S.fromList allValues
  , _cellGroups = S.fromList . map GroupId $ groupIds
  }

-- Monoid instance for this and Group are cheats - we're using it plus _Just in
-- lenses to avoid handling error cases where either cell or group ID don't
-- exist in the problem.
instance Semigroup Cell where
  a <> b = error "don't use this"

instance Monoid Cell where
  mempty = mkCell "" []

buildSudoku :: Eff '[ State Problem ] m -> Problem
buildSudoku m = runApp (Problem { _problemCells = state, _problemStructure = structure }) m

-- This is gross, will learn more with new types of groups (e.g. sandwich)
getCells (AllValues xs) = toList xs
getCells (Sandwich _ xs) = xs

constrain :: AppEff effs => CellId -> Constraint -> Eff effs ()
constrain cid value = do
  existing <- getConstraint cid

  when (existing /= value) $ do
    assign (problemCells . at cid . _Just . cellConstraint) value

    groupIds <- gets . view $ problemCells . at cid . _Just . cellGroups

    -- Rule: if N cells in a group are constrained to the same N values, then
    -- no other cell in that group can contain those values.
    --
    -- Interesting that N=1 ("specify a number") is a specialization of a more
    -- general rule! (N=2 covers "twins")
    --
    -- TODO: can probably generalize further to cover hidden twins as well.
    forM_ groupIds $ \groupId -> do
      -- TODO: provide structure as a separate read only effect
      group <- gets . view $ problemStructure . at groupId . _Just

      case group of
        AllValues xs -> do
          let cellIds = toList xs
          cells <- mapM (\x -> gets . view $ problemCells . at x . _Just) cellIds

          let matching = filter (\cell -> view cellConstraint cell == value) cells

          when (length matching == (length . toList $ value)) $ do
            forM_ (cells \\ matching) $ \cell -> do
              constrain
                (view cellId cell)
                (S.difference (view cellConstraint cell) value)
        Sandwich t xs -> do
          if toList value `elem` subsequences [1,4] then
              do
                let components = sortOn length . filter ((==) t . sum) . subsequences $ allValues
                -- For forwards direction, find min & maximum length (given remaining cells)
                let lengths = map length components
                let mn = minimum lengths
                let mx = maximum lengths
                -- Restrict outside that bounds "can't be max value"



                -- Filter components to <= max length
                -- Calculate possibleValues for cell (MustBe)
                let possibleValues = foldl' (\a v -> zipWith (++) (replicate (length v) v) (a ++ repeat [])) [[]] $ components
                -- Constraint them
                -- Find the current cell in xs
                -- then send possible values forward and backward
                return ()
          else
            return ()

          -- If cell is min/max, enumerate the different ways to make the sum, then constraint cells before/after it

getConstraint :: AppEff effs => CellId -> Eff effs Constraint
getConstraint cid =
  gets $ view (problemCells . at cid . _Just . cellConstraint)

given :: AppEff effs => T.Text -> Int -> Eff effs ()
given cid value = do
  constrain (CellId cid) (S.singleton value)

formatCell (Cell { _cellId = CellId cid, _cellConstraint = constraint}) =
  cid <> " " <> (T.pack . show $ constraint)

--state =
--  M.fromList . map (\x -> (_cellId x, x)) $
--  [ mkCell "r1c1" ["r1", "s1"]
--  , mkCell "r1c2" ["r1", "s1"]
--  , mkCell "r1c3" ["r1", "s2"]
--  , mkCell "r1c4" ["r1", "s2"]
--  , mkCell "r2c1" ["r2", "s1"]
--  , mkCell "r2c2" ["r2", "s1"]
--  , mkCell "r2c3" ["r2", "s2"]
--  , mkCell "r2c4" ["r2", "s2"]
--  ]
--
--structure = M.fromList
--  [ (GroupId "r1", AllValues $ S.fromList . map CellId $ ["r1c1", "r1c2", "r1c3", "r1c4"])
--  , (GroupId "r2", AllValues $ S.fromList . map CellId $ ["r2c1", "r2c2", "r2c3", "r2c4"])
--  , (GroupId "s1", AllValues $ S.fromList . map CellId $ ["r1c1", "r1c2", "r2c1", "r2c2"])
--  , (GroupId "s2", AllValues $ S.fromList . map CellId $ ["r2c3", "r2c4", "r2c3", "r2c4"])
--  ]
state =
  M.fromList . map (\x -> (_cellId x, x)) $
  [ mkCell "r1c1" ["r1", "s1"]
  , mkCell "r1c2" ["r1", "s1"]
  , mkCell "r1c3" ["r1", "s1"]
  , mkCell "r1c4" ["r1", "s1"]
  ]

structure = M.fromList
  [ (GroupId "r1", AllValues $ S.fromList . map CellId $ ["r1c1", "r1c2", "r1c3", "r1c4"])
  , (GroupId "s1", Sandwich 2 $ map CellId ["r1c1", "r1c2", "r1c3", "r1c4"])
  ]

main = do
  --let problem =
  --      buildSudoku $ do
  --        given "r2c1" 1
  --        given "r2c2" 2
  --        given "r1c4" 1
  let problem =
        buildSudoku $ do
          given "r1c1" 1

  forM_ (sortOn (show . _cellId) . toList $ (view problemCells problem)) $ \x -> do
    putStrLn . T.unpack . formatCell $ x
