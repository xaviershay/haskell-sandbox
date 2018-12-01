{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Maybe (fromJust)
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
import Data.IORef

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
  , _life :: M.HashMap Player Int
  } deriving (Show)

type GameMonad a = (ExceptT String (StateT Board Identity)) a

makeLenses ''Board
makeLenses ''Card

setAttribute :: CardAttribute -> Card -> Card
setAttribute attr = over cardAttributes (S.insert attr)

removeAttribute :: CardAttribute -> Card -> Card
removeAttribute attr = over cardAttributes (S.delete attr)

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

requireName :: CardName -> CardMatcher
requireName n = CardMatcher $ (==) n . view cardName

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

  when
    (hasAttribute "sorcery" card || hasAttribute "instant" card) $
    modifying
      (counters . at "storm" . non 0)
      (+ 1)

  modifying
    stack
    (\s -> (name, True) : s)

jumpstart name discard = do
  -- TODO: Discard
  card <- requireCard name (requireLocation (Active, Graveyard))

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
        c <- requireCard name mempty

        assign stack ss

        if hasAttribute "creature" c then
          modifying
            (cards . at name . _Just)
            (setAttribute "summoned" . set location (Active, Play))
        else
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

validate targetName reqs = do
  _ <- requireCard targetName reqs
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

sacrifice targetName = do
  card <- requireCard targetName inPlay

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

attackWith :: [CardName] -> GameMonad ()
attackWith cs = do
  forM_ cs $ \cn -> do
    c <- requireCard cn
           (requireLocation (Active, Play)
             <> requireAttribute "creature"
             <> missingAttribute "summoned")
    tap cn
    modifying
      (life . at Opponent . non 0)
      (\x -> x - view (cardStrength . _1) c)

numbered n name = name <> " " <> show n
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

      if hasAttribute "lifelink" cx then
        do
          let owner = fst . view location $ cx
          modifying (life . at owner . non 0) (+ xdmg)
      else
        return ()

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

createToken name strength location = do
  board <- get

  let board' = execState (
                  addCreature name strength location ["creature", "token", "summoned"]
                ) board
  put board'

runMonad :: Board -> GameMonad () -> (Either String (), Board)
runMonad state m =
  runIdentity $ runStateT (runExceptT m) state

-- https://stackoverflow.com/questions/15412027/haskell-equivalent-to-scalas-groupby
groupByWithKey :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
groupByWithKey f = map (f . head &&& id)
                   . groupBy ((==) `on` f)
                   . sortBy (compare `on` f)

printBoard board = do
  putStr "Opponent Life: "
  putStrLn . show $ view (life . at Opponent . non 0) board
  putStr "Storm: "
  putStrLn . show $ view (counters . at "storm" . non 0) board
  putStrLn ""
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
    forM_ (view stack board) $ \(cn, cast) -> do
      putStrLn $ "  " <> cn <> (if cast then "" else " (copy)")

addCardFull name strength loc attrs = do
  let c = set cardStrength strength $ set cardAttributes (S.fromList attrs) $ mkCard name loc

  modifying cards (M.insert name c)

addCard name loc attrs = do
  addCardFull name (0, 0) loc attrs

addCreature :: CardName -> (Int, Int) -> CardLocation -> [CardAttribute] -> State Board ()
addCreature name strength loc attrs =
  addCardFull name strength loc ("creature":attrs)

addCards 0 name loc attrs = return ()
addCards n name loc attrs = do
  addCard (name <> " " <> show n) loc attrs
  addCards (n - 1) name loc attrs

setLife p n =
  assign (life . at p) (Just n)

buildBoard m = execState m $ Board
                 { _cards = mempty
                 , _counters = mempty
                 , _stack = mempty
                 , _life = mempty
                 }

initialBoardState = buildBoard $ do
  -- Hand
  addCard "Undercity Uprising" (Active, Hand) ["sorcery"]
  addCard "Doublecast" (Active, Hand) ["sorcery"]
  addCard "Plummet" (Active, Hand) ["instant"]
  addCard "Quasiduplicate" (Active, Hand) ["sorcery"]
  addCreature "Torgaar, Famine Incarnate" (7, 6) (Active, Hand) ["creature"]

  -- Play
  addCard "Thousand-Year Storm" (Active, Play) []
  addCreature "Adeliz, the Cinder Wind" (4, 4) (Active, Play) ["flying"]
  addCreature "Afzocan Archer" (1, 4) (Active, Play) []
  addCards 4 "Timber Gorge" (Active, Play) []
  addCards 4 "Submerged Boneyard" (Active, Play) []
  addCards 4 "Highland Lake" (Active, Play) []

  -- Opponent
  addCreature "Kopala, Warden of Waves" (2, 2) (Opponent, Play) ["merfolk"]
  addCreature "Merfolk Mistbinder 1" (2, 2) (Opponent, Play) ["merfolk"]
  addCreature "Merfolk Mistbinder 2" (3, 3) (Opponent, Play) ["merfolk"]
  addCreature "Shalai, Voice of Plenty" (3, 4) (Opponent, Play) ["angel", "flying"]
  addCreature "Lyra Dawnbringer" (5, 5) (Opponent, Play) ["angel", "flying", "lifelink"]
  addCreature "Angel 1" (4, 4) (Opponent, Play) ["angel", "flying", "token"]
  addCreature "Angel 2" (4, 4) (Opponent, Play) ["angel", "flying", "token"]
  addCreature "Angel 3" (4, 4) (Opponent, Play) ["angel", "flying", "token"]

  setLife Opponent 12

  -- Get current board
main :: IO ()
main = do
  let (_, board) = runMonad initialBoardState $ do
                     -- Apply Shalai's hexproof effect
                     forCards
                       (requireAttribute "creature"
                          <> requireLocation (Opponent, Play)
                          <> (invert $ requireName "Shalai, Voice of Plenty"))
                       (setAttribute "hexproof")

                     -- Apply Lyra's +1/+1 and lifelink
                     forCards
                       (requireAttribute "creature"
                          <> requireLocation (Opponent, Play)
                          <> requireAttribute "angel"
                          <> (invert $ requireName "Lyra Dawnbringer"))
                       (setAttribute "lifelink" . over cardStrength (\(a, b) -> (a + 1, b + 1)))

                     -- Apply Mistbinder's +1/+1
                     forCards
                       (requireAttribute "creature"
                          <> requireLocation (Opponent, Play)
                          <> requireAttribute "merfolk"
                          <> (invert $ requireName "Merfolk Mistbinder 1"))
                       (over cardStrength (\(a, b) -> (a + 1, b + 1)))

                     forCards
                       (requireAttribute "creature"
                          <> requireLocation (Opponent, Play)
                          <> requireAttribute "merfolk"
                          <> (invert $ requireName "Merfolk Mistbinder 2"))
                       (over cardStrength (\(a, b) -> (a + 1, b + 1)))


  --putStrLn . show $ e
  --putStrLn ""

  --printBoard newBoard3

  b <- newIORef (1, board)
  let step = \desc m -> do
                (n, board) <- readIORef b

                putStr $ show n <> ". "
                putStrLn desc
                let (e, board') = runMonad board m

                case e of
                  Left e -> do
                    putStrLn e
                    return () -- TODO: Exit
                  Right _ -> do
                    printBoard board'
                    putStrLn ""
                    putStrLn ""
                    writeIORef b (n+1, board')

  step "Use Undercity Uprising on Adeliz to destroy Shalai" $ do
    tap $ numbered 1 "Timber Gorge"
    tap $ numbered 1 "Submerged Boneyard"
    tap $ numbered 2 "Submerged Boneyard"
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
    validate "Shalai, Voice of Plenty" $ requireLocation (Opponent, Graveyard)

    forCards
      (requireAttribute "creature" <> requireLocation (Opponent, Play))
      (removeAttribute "hexproof")

  step "Cast Doublecast" $ do
    tap $ numbered 2 "Timber Gorge"
    tap $ numbered 3 "Timber Gorge"
    cast "Doublecast"
    trigger "Thousand-Year Storm"
    storm $ const (copy "Doublecast")

    trigger "Adeliz, the Cinder Wind"
    modifyStrength "Adeliz, the Cinder Wind" (1, 1)

    resolve "Doublecast"
    resolve "Doublecast"

  step "Cast Plummet to destroy all fliers" $ do
    tap "Timber Gorge 4"
    cast "Plummet"
    trigger "Thousand-Year Storm"
    storm $ const (copy "Plummet")

    trigger "Adeliz, the Cinder Wind"
    modifyStrength "Adeliz, the Cinder Wind" (1, 1)

    -- From double doublecast earlier
    copy "Plummet"
    copy "Plummet"

    resolve "Plummet"
    target "Lyra Dawnbringer"
    destroy "Lyra Dawnbringer"
    validate "Lyra Dawnbringer" $ requireLocation (Opponent, Graveyard)
    forCards
      (requireAttribute "creature"
         <> requireLocation (Opponent, Play)
         <> requireAttribute "angel"
         <> (invert $ requireName "Lyra Dawnbringer"))
      (removeAttribute "lifelink" . over cardStrength (\(a, b) -> (a - 1, b - 1)))

    forM_ [1..3] $ \n -> do
      resolve "Plummet"
      target $ numbered n "Angel"
      destroy $ numbered n "Angel"

    resolve "Plummet" -- No target

  step "Quasiduplicate on archer, destroy one of the Mistbinders" $ do
    tap $ numbered 1 "Highland Lake"
    tap $ numbered 2 "Highland Lake"
    cast "Quasiduplicate"
    storm $ const (copy "Quasiduplicate")
    trigger "Adeliz, the Cinder Wind"
    modifyStrength "Adeliz, the Cinder Wind" (1, 1)

    forM_ [1..4] $ \n -> do
      let tokenName = ("Afzocan Archer " <> show n)
      resolve "Quasiduplicate"
      createToken tokenName (1, 4) (Active, Play)
      fight tokenName $ numbered 2 "Merfolk Mistbinder"

    validate (numbered 2 "Merfolk Mistbinder") $
      requireLocation (Opponent, Graveyard)
    forCards
      (requireAttribute "creature"
         <> requireLocation (Opponent, Play)
         <> requireAttribute "merfolk"
         <> (invert . requireName $ numbered 2 "Merfolk Mistbinder"))
      (over cardStrength (\(a, b) -> (a - 1, b - 1)))

  step "Jump-start Quasiduplicate again (w/ Waterknot), destroy merfolk" $ do
    tap $ numbered 3 "Highland Lake"
    tap $ numbered 4 "Highland Lake"
    jumpstart "Quasiduplicate" "Waterknot"
    storm $ const (copy "Quasiduplicate")
    trigger "Adeliz, the Cinder Wind"
    modifyStrength "Adeliz, the Cinder Wind" (1, 1)

    forM_ [1..2] $ \n -> do
      let tokenName = ("Afzocan Archer " <> show n)
      resolve "Quasiduplicate"
      createToken tokenName (1, 4) (Active, Play)
      fight tokenName "Merfolk Mistbinder 1"

    validate (numbered 1 "Merfolk Mistbinder") $
      requireLocation (Opponent, Graveyard)

    forCards
      (requireAttribute "creature"
         <> requireLocation (Opponent, Play)
         <> requireAttribute "merfolk"
         <> (invert $ requireName $ numbered 1 "Merfolk Mistbinder"))
      (over cardStrength (\(a, b) -> (a - 1, b - 1)))

    forM_ [3..4] $ \n -> do
      let tokenName = numbered n "Afzocan Archer"
      resolve "Quasiduplicate"
      createToken tokenName (1, 4) (Active, Play)
      fight tokenName "Kopala, Warden of Waves"

    forM_ [5] $ \n -> do
      let tokenName = numbered n "Afzocan Archer"
      resolve "Quasiduplicate"
      createToken tokenName (1, 4) (Active, Play)

  step "Torgaar, sacrificing archers to reduce cost" $ do
    tap $ numbered 3 "Submerged Boneyard"
    tap $ numbered 4 "Submerged Boneyard"
    sacrifice $ numbered 1 "Afzocan Archer"
    sacrifice $ numbered 2 "Afzocan Archer"
    sacrifice $ numbered 3 "Afzocan Archer"
    cast "Torgaar, Famine Incarnate"
    resolve "Torgaar, Famine Incarnate"
    setLife Opponent 10

  step "Attack with Adeliz and initial archer for lethal" $ do
    attackWith ["Adeliz, the Cinder Wind", "Afzocan Archer"]
