{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Debug.Trace

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
  , _cardStrength :: (Int, Int)
  , _cardDamage :: Int
  } deriving (Show)

data Board = Board
  { _cards :: M.HashMap CardName Card
  , _stack :: [Spell]
  , _counters :: M.HashMap String Int
  } deriving (Show)

type GameMonad a = (ExceptT String (StateT Board Identity)) a

makeLenses ''Board
makeLenses ''Card

setAttribute :: CardAttribute -> Card -> Card
setAttribute attr = over cardAttributes (S.insert attr)

hasAttribute attr = S.member attr . view cardAttributes

mkCard name location =
  Card
    { _cardName = name
    , _location = location
    , _cardAttributes = mempty
    , _cardStrength = (0, 0)
    , _cardDamage = 0
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

modifyStrength :: CardName -> (Int, Int) -> GameMonad ()
modifyStrength cn (x, y) = do
  c <- requireCard cn (inPlay <> requireAttribute "creature")

  let c' = over cardStrength (\(a, b) -> (a + x, b + y)) c

  assign
    (cards . at cn . _Just)
    c'


fight :: CardName -> CardName -> GameMonad ()
fight x y = do
  _ <- requireCard x inPlay
  _ <- requireCard y inPlay

  fight' x y
  fight' y x

  where
    fight' x y = do
      cx <- requireCard x (requireAttribute "creature")
      cy <- requireCard y (requireAttribute "creature")

      let xdmg = max 0 $ view (cardStrength . _1) cx
      let cy' = over cardDamage (+ xdmg) cy

      assign
        (cards . at y . _Just)
        cy'

      if view cardDamage cy' >= view (cardStrength . _2) cy' || (xdmg > 0 && hasAttribute "deathtouch" cx ) then
        destroy y
      else
        return ()

forCards :: CardMatcher -> (Card -> Card) -> GameMonad ()
forCards matcher f = do
  cs <- use cards

  let matchingCs = filter (applyMatcher matcher) (M.elems cs)

  forM_ matchingCs $ \c ->
    assign
      (cards . at (view cardName c) . _Just)
      (f c)

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
    forM_ (sortBy (compare `on` view cardName) cs) $ \c -> do
      putStrLn $ "  " <> view cardName c <>
        " (" <> (intercalate "," . sort . S.toList $ view cardAttributes c) <> ")"
        <> if hasAttribute "creature" c then
             " (" <> (show $ view (cardStrength . _1) c) <> "/" <> (show $ view (cardStrength . _2) c) <> ", " <> (show $ view cardDamage c) <> ")"
           else
            ""

  when (not . null $ view stack board) $ do
    putStrLn "Stack"
    forM_ (view stack board) $ \(cn, copy) -> do
      putStrLn $ "  " <> cn <> (if copy then " (copy)" else "")

--addCard :: MonadState m a => CardName -> CardLocation -> [CardAttribute] -> m
addCardFull name strength loc attrs = do
  let c = set cardStrength strength $ set cardAttributes (S.fromList attrs) $ mkCard name loc

  modifying cards (M.insert name c)

addCard name loc attrs = do
  addCardFull name (0, 0) loc attrs

addCreature name strength loc attrs =
  addCardFull name strength loc ("creature":attrs)

addCards 0 name loc attrs = return ()
addCards n name loc attrs = do
  addCard (name <> " " <> show n) loc attrs
  addCards (n - 1) name loc attrs

buildBoard m = execState m $ Board
                 { _cards = mempty
                 , _counters = mempty
                 , _stack = mempty
                 }

initialBoardState = buildBoard $ do
  -- Hand
  addCard "Undercity Uprising" (Active, Hand) []

  -- Play
  addCard "Thousand-Year Storm" (Active, Play) []
  addCreature "Adeliz, the Cinder Wind" (4, 4) (Active, Play) ["legendary", "flying"]
  addCreature "Afzocan Archer" (1, 4) (Active, Play) []
  addCards 4 "Timber Gorge" (Active, Play) []
  addCards 4 "Submerged Boneyard" (Active, Play) []
  addCards 4 "Highland Lake" (Active, Play) []

  -- Opponent
  addCreature "Shalai, Voice of Plenty" (3, 4) (Opponent, Play) ["flying", "legendary"]

main :: IO ()
main = do
  let board = initialBoardState

  let (e, newBoard) =
                      runMonad board $ do
                         tap "Timber Gorge 1"
                         tap "Submerged Boneyard 1"
                         tap "Submerged Boneyard 2"
                         cast "Undercity Uprising"
                         trigger "Thousand-Year Storm"
                         storm $ const (copy "Undercity Uprising")

                         trigger "Adeliz, the Cinder Wind"
                         modifyStrength "Adeliz, the Cinder Wind" (1, 1)

                         resolve "Undercity Uprising"
                         forCards
                           (requireAttribute "creature" <> requireLocation (Active, Play))
                           (setAttribute "deathtouch")
                         target "Adeliz, the Cinder Wind"
                         target "Shalai, Voice of Plenty"
                         fight "Adeliz, the Cinder Wind" "Shalai, Voice of Plenty"

  putStrLn . show $ e
  putStrLn ""

  printBoard newBoard
