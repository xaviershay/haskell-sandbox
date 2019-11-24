-- Really terrible PoC code messing around with solving sudoku problems without dynamic programming

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict ((!))
import Data.List (foldl')
import Data.Foldable (toList)
import qualified Data.Text as T
import Control.Monad (forM_)
import Data.Hashable
import GHC.Generics
import Debug.Trace
import Data.Sort (sortOn)

newtype GroupId = GroupId T.Text deriving (Eq, Generic, Show)
newtype CellId = CellId T.Text deriving (Eq, Generic, Show)

instance Hashable CellId
instance Hashable GroupId

-- TODO: Probably don't need a datatype here
data Constraint =
  MustBe (S.HashSet Int)
  deriving (Show, Eq)

data Cell = Cell
  { cellId :: CellId
  , cellConstraint :: Constraint
  , cellGroups :: S.HashSet GroupId
  } deriving (Show)

instance Eq Cell where
  Cell { cellId = a } == Cell { cellId = b } = a == b

instance Hashable Cell where
  hashWithSalt n (Cell {cellId = a}) = hashWithSalt n a

data Group = AllValues (S.HashSet CellId) deriving (Show)

type State = M.HashMap CellId Cell
type Structure = M.HashMap GroupId Group

instance Semigroup Constraint where
  MustBe a <> MustBe b = MustBe (S.intersection a b)

allValues = [1..4]

instance Monoid Constraint where
  mempty = MustBe (S.fromList allValues)

-- TODO: implies types aren't right yet
constraintValues (MustBe xs) = xs

mkCell :: T.Text -> [T.Text] -> Cell
mkCell name groupIds = Cell
  { cellId = CellId name
  , cellConstraint = mempty
  , cellGroups = S.fromList . map GroupId $ groupIds
  }

state =
  constrain (CellId "r2c1") (MustBe (S.singleton 1)) $
  constrain (CellId "r2c2") (MustBe (S.singleton 2)) $
  constrain (CellId "r1c4") (MustBe (S.singleton 1)) $
  M.fromList . map (\x -> (cellId x, x)) $
  [ mkCell "r1c1" ["r1", "s1"]
  , mkCell "r1c2" ["r1", "s1"]
  , mkCell "r1c3" ["r1", "s2"]
  , mkCell "r1c4" ["r1", "s2"]
  , mkCell "r2c1" ["r2", "s1"]
  , mkCell "r2c2" ["r2", "s1"]
  , mkCell "r2c3" ["r2", "s2"]
  , mkCell "r2c4" ["r2", "s2"]
  ]

structure = M.fromList
  [ (GroupId "r1", AllValues $ S.fromList . map CellId $ ["r1c1", "r1c2", "r1c3", "r1c4"])
  , (GroupId "r2", AllValues $ S.fromList . map CellId $ ["r2c1", "r2c2", "r2c3", "r2c4"])
  , (GroupId "s1", AllValues $ S.fromList . map CellId $ ["r1c1", "r1c2", "r2c1", "r2c2"])
  , (GroupId "s2", AllValues $ S.fromList . map CellId $ ["r2c3", "r2c4", "r2c3", "r2c4"])
  ]

formatCell (Cell { cellId = CellId cid, cellConstraint = constraint}) =
  cid <> " " <> (T.pack . show $ constraint)

constrain :: CellId -> Constraint -> State -> State
constrain cid constraint state =
  let existing = state ! cid in

  if cellConstraint existing == constraint then
    state
  else
    let state' = M.adjust (\c -> c { cellConstraint = constraint }) cid state in
    let newCell = state' ! cid in

    case (cellConstraint newCell) of
      MustBe xs ->
        let n = length . S.toList $ xs in
        let groups =
                      map (\x -> structure ! x)
                    $ (S.toList $ cellGroups newCell) in
        let x = groups :: [Group] in

        let filtered = filter (hasNCellsMatchingConstraint n (cellConstraint newCell) state') (trace (show groups) groups) in

        -- WANT: Groups where length of cells with same constraint == n
        let state'' = foldl' (applyF $ cellConstraint newCell) state' (trace (show filtered) filtered) in

        state''

-- Every cell that doesn't match constraint, subtract constraint from
applyF :: Constraint -> State -> Group -> State
applyF constraint state group =
  case group of
    AllValues cs ->
      foldl' applyG state (S.toList cs)

  where
    applyG :: State -> CellId -> State
    applyG state cid =
      let cell = state ! cid in

      if constraint == cellConstraint cell then
        state
      else
        constrain cid (MustBe $ S.difference (constraintValues . cellConstraint $ cell) (constraintValues constraint)) state

hasNCellsMatchingConstraint n constraint state group =
  case group of
    AllValues cs ->
      let cellsInGroup = S.toList $ S.map (\c -> state ! c) cs in
      let filtered = filter (\c -> trace (show $ (c, constraint)) (cellConstraint c == constraint)) cellsInGroup in

      length filtered == n

main =
  forM_ (sortOn (show . cellId) . toList $ state) $ \x -> do
    putStrLn . T.unpack . formatCell $ x
