{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Lens
import Data.Monoid
import qualified Data.HashMap.Strict as M
import qualified Data.Set as S
import Data.Hashable
import GHC.Generics hiding (to)
import           Control.Monad.Except
import           Control.Monad.State hiding (state)
import Control.Arrow ((&&&))
import Data.List
import Data.Function

type CardName = String
type CardAttribute = String
type CardLocation = (Player, Location)
type Spell = (CardName, Bool) -- Bool = Whether spell was cast, false if copy

data Player = Active | Opponent deriving (Show, Eq, Generic, Ord)
data Location = Hand | Graveyard | Play | Playing | Exile deriving (Show, Eq, Ord)
data CardState = Tapped | Untapped deriving (Show)

instance Hashable Player

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardAttributes :: S.Set CardAttribute
  } deriving (Show)

data Board = Board
  { _cards :: M.HashMap CardName Card
  , _stack :: [Spell]
  , _counters :: M.HashMap String Int
  } deriving (Show)

type GameMonad a = (ExceptT String (StateT Board Identity)) a

makeLenses ''Board
makeLenses ''Card

setAttribute attr = over cardAttributes (S.insert attr)

mkCard name location =
  Card
    { _cardName = name
    , _location = location
    , _cardAttributes = mempty
    }

requireCard :: CardName -> CardMatcher -> GameMonad Card
requireCard name f = do
  board <- get

  case view (cards . at name) board of
    Nothing -> throwError $ "Card does not exist: " <> name
    Just card -> if applyMatcher f card then
                   return card
                 else
                   throwError $ "Card does not match requirements: " <> name
  
data CardMatcher = CardMatcher (Card -> Bool)

requireLocation :: CardLocation -> CardMatcher
requireLocation loc = CardMatcher $ (==) loc . view location

inPlay = CardMatcher $ \c -> case view location c of
                               (_, Play) -> True
                               _         -> False

requireAttribute :: CardAttribute -> CardMatcher
requireAttribute attr = CardMatcher $ S.member attr . view cardAttributes

missingAttribute = invert . requireAttribute

invert :: CardMatcher -> CardMatcher
invert (CardMatcher f) = CardMatcher $ not . f

applyMatcher :: CardMatcher -> Card -> Bool
applyMatcher (CardMatcher f) c = f c

instance Monoid CardMatcher where
  mempty = CardMatcher $ const True
  mappend (CardMatcher f) (CardMatcher g) = CardMatcher $ \c -> f c && g c

tap :: CardName -> GameMonad ()
tap name = do
  let tapA = "tapped"

  card <- requireCard name
    (requireLocation (Active, Play) <> missingAttribute tapA)

  modifying
    (cards . at name . _Just . cardAttributes)
    (S.insert tapA)

cast name = do
  card <- requireCard name (requireLocation (Active, Hand))

  assign
    (cards . at name . _Just . location)
    (Active, Playing)

  modifying
    (counters . at "storm" . non 0)
    (+ 1)

  modifying
    stack
    (\s -> (name, True) : s)

resolve :: CardName -> GameMonad ()
resolve expectedName = do
  s <- use stack

  case s of
    [] -> throwError $ "No spell on stack to resolve for: " <> expectedName
    ((name, cast):ss) ->

      if name /= expectedName then
        throwError $ "Top spell on stack does not match: " <> name
      else do
        assign stack ss

        if cast then
          assign
            (cards . at name . _Just . location)
            (Active, Graveyard)
        else
          return ()
          
target targetName = do
  card <- requireCard targetName (inPlay <> missingAttribute "hexproof")

  return ()

trigger targetName = do
  -- TODO: Technically some cards can trigger from other zones, figure out best
  -- way to represent.
  card <- requireCard targetName inPlay

  return ()

destroy targetName = do
  card <- requireCard targetName (inPlay <> missingAttribute "indestructible")

  case S.member "token" (view cardAttributes card) of
    True -> modifying cards (M.delete targetName)
    False -> 
          modifying
            (cards . at targetName . _Just . location)
            (\(x, _) -> (x, Graveyard))

  return ()

copy targetName = do
  card <- requireCard targetName mempty

  modifying
    stack
    (\s -> (targetName, False) : s)
  
storm :: (Int -> GameMonad ()) -> GameMonad ()
storm action = do
  maybeStorm <- use $ counters . at "storm"

  case maybeStorm of
    Nothing -> throwError $ "No counter in state: storm"
    Just c -> forM [1..c-1] $ \n -> action n

  return ()

runMonad :: Board -> GameMonad () -> (Either String (), Board)
runMonad state m =
  runIdentity $ runStateT (runExceptT m) state

-- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
groupByWithKey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupByWithKey f = map (f . head &&& id)
                   . groupBy ((==) `on` f)
                   . sortBy (compare `on` f)

printBoard board = do
  let sections = groupByWithKey (view location) (M.elems $ view cards board)

  forM_ sections $ \(loc, cs) -> do
    putStrLn . show $ loc
    forM_ cs $ \c -> do
      putStrLn $ "  " <> view cardName c <> " (" <> (intercalate "," . sort . S.toList $ view cardAttributes c) <> ")"

  when (not . null $ view stack board) $ do
    putStrLn "Stack"
    forM_ (view stack board) $ \(cn, copy) -> do
      putStrLn $ "  " <> cn <> (if copy then " (copy)" else "")

main :: IO ()
main = do
  let cards =
              [ setAttribute "token" $ mkCard "Angel Token 1" (Opponent, Play)
              , setAttribute "token" $ mkCard "Angel Token 2" (Opponent, Play)
              , setAttribute "token" $ mkCard "Angel Token 3" (Opponent, Play)
              , mkCard "Thousand-Year Storm" (Active, Play)
              , mkCard "Plummet" (Active, Hand)
              , mkCard "Timber Gorge 1" (Active, Play)
              ]

  let board = Board
                { _cards = M.fromList (map (\x -> (view cardName x, x)) cards)
                , _counters = mempty
                , _stack = mempty
                }

  let (e, newBoard) =
                      runMonad board $ do
                         tap "Timber Gorge 1"
                         cast "Plummet"
                         trigger "Thousand-Year Storm"
                         storm $ const (copy "Plummet")

                         resolve "Plummet"
                         target "Angel Token 1"
                         destroy "Angel Token 1"

  putStrLn . show $ e
  putStrLn ""
  printBoard newBoard
