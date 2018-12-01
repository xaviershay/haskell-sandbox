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
import           Control.Monad.Writer
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

data Effect = Effect (Card -> Card) (Card -> Card)

instance Show Effect where
  show _ = "<effect>"

instance Monoid Effect where
  mempty = Effect id id
  mappend (Effect f1 g1) (Effect f2 g2) = Effect (f1 . f2) (g1 . g2)

data Card = Card
  { _cardName :: CardName
  , _location :: (Player, Location)
  , _cardAttributes :: S.Set CardAttribute
  , _cardStrength :: (Int, Int)
  , _cardDamage :: Int
  } deriving (Show)

data CardMatcher = CardMatcher (Card -> Bool)

instance Show CardMatcher where
  show _ = "<matcher>"

data Board = Board
  { _cards :: M.HashMap CardName Card
  , _stack :: [Spell]
  , _counters :: M.HashMap String Int
  , _life :: M.HashMap Player Int
  , _effects :: M.HashMap String (CardMatcher, Effect)
  } deriving (Show)

type GameMonad a = (ExceptT String (StateT Board (WriterT [(String, Board)] Identity))) a

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


attributeEffect attr = Effect (setAttribute attr) (removeAttribute attr)
strengthEffect (x, y) = Effect
  (over cardStrength (\(a, b) -> (a + x, b + y)))
  (over cardStrength (\(a, b) -> (a - x, b - y)))
requireLocation :: CardLocation -> CardMatcher
requireLocation loc = CardMatcher $ (==) loc . view location

inPlay = CardMatcher $ \c -> case view location c of
                               (_, Play) -> True
                               _         -> False

requireAttribute :: CardAttribute -> CardMatcher
requireAttribute attr = CardMatcher $ S.member attr . view cardAttributes

requireName :: CardName -> CardMatcher
requireName n = CardMatcher $ (==) n . view cardName

requireOther = invert . requireName

requireController player = CardMatcher $ (==) player . view (location . _1)

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

castFromLocation name loc = do
  card <- requireCard name (requireLocation loc)

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

jumpstart discard name = do
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

        when (hasAttribute "creature" c) $
          modifying
            (cards . at name . _Just)
            (setAttribute "summoned")

        if (hasAttribute "sorcery" c || hasAttribute "instant" c) then
          when cast $
            assign
              (cards . at name . _Just . location)
              (Active, Graveyard)
        else
          modifying
            (cards . at name . _Just)
            (set location (Active, Play))

target targetName = do
  card <- requireCard targetName (inPlay <> missingAttribute "hexproof")

  return ()

targetInLocation targetName zone = do
  card <- requireCard targetName (requireLocation zone)

  return ()

trigger targetName = do
  -- TODO: Technically some cards can trigger from other zones, figure out best
  -- way to represent.
  card <- requireCard targetName inPlay

  return ()

validateRemoved targetName = do
  board <- get
  case view (cards . at targetName) board of
    Nothing -> return ()
    Just _ -> throwError $ "Card should be removed: " <> targetName

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

resetStrength :: CardName -> (Int, Int) -> GameMonad ()
resetStrength cn desired = do
  c <- requireCard cn (requireAttribute "creature")

  let c' = set cardStrength desired c

  assign
    (cards . at cn . _Just)
    c'

moveToGraveyard cn = do
  c <- requireCard cn mempty

  let c' = over location (\(player, _) -> (player, Graveyard)) c

  assign
    (cards . at cn . _Just)
    c'

modifyStrength :: CardName -> (Int, Int) -> GameMonad ()
modifyStrength cn (x, y) = do
  c <- requireCard cn (inPlay <> requireAttribute "creature")

  let c' = over cardStrength (\(a, b) -> (a + x, b + y)) c

  -- TODO: Handle tokens
  let c'' =
        if view (cardStrength . _2) c' <= 0 then
          over location (\(player, _) -> (player, Graveyard)) c'
        else
          c'

  assign
    (cards . at cn . _Just)
    c''

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

  target x
  target y

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

runMonad :: Board -> GameMonad () -> (Either String (), Board, [(String, Board)])
runMonad state m =
  let ((e, b), log) = runIdentity $
                        runWriterT (runStateT (runExceptT m) state) in

  (e, b, log)

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

addCreature name strength loc attrs =
  addCardFull name strength loc ("creature":attrs)

addCards 0 name loc attrs = return ()
addCards n name loc attrs = do
  addCard (name <> " " <> show n) loc attrs
  addCards (n - 1) name loc attrs

setLife p n =
  assign (life . at p) (Just n)

emptyBoard = Board
               { _cards = mempty
               , _counters = mempty
               , _stack = mempty
               , _life = mempty
               , _effects = mempty
               }

addEffect cn f effect = do
  modifying effects (M.insert cn (f, effect))

with x f = f x

requireEffect effectName = do
  board <- get
  case view (effects . at effectName) board of
    Nothing -> throwError $ "No effect named: " <> effectName
    Just x -> return x

applyEffect effectName = do
  (matcher, Effect forward _) <- requireEffect effectName

  forCards matcher forward

removeEffect effectName = do
  (matcher, Effect _ undo) <- requireEffect effectName

  forCards matcher undo

step desc m = do
  b <- get
  let (e, b', _) = runMonad b m

  tell [(desc, b')]
  put b'

  case e of
    Left x -> throwError x
    Right _ -> return ()

gainLife player amount =
  modifying
    (life . at player . non 0)
    (\x -> x + amount)

loseLife player amount =
  modifying
    (life . at player . non 0)
    (\x -> x - amount)

main :: IO ()
main = do
  let (e, _, log) = runMonad emptyBoard guilds_of_ravnica_8

  forM_ (zip log [1..]) $ \((step, board), n) -> do
    putStr $ (show n) <> ". "
    putStrLn $ step
    putStrLn ""
    printBoard board
    putStrLn ""
    putStrLn ""

  case e of
    Left x -> fail x
    Right _ -> return ()

returnToHand cn =
  assign
    (cards . at cn . _Just . location)
    (Active, Hand)

returnToPlay cn =
  assign
    (cards . at cn . _Just . location)
    (Active, Play)

-- http://www.possibilitystorm.com/089-guilds-of-ravnica-season-puzzle-7-2/
guilds_of_ravnica_8 = do
  setLife Opponent 7

  addCreature "Epicure of Blood" (4, 4) (Active, Play) []
  addCreature "Muldrotha, the Gravetide" (6, 6) (Active, Play) []
  addCreature "Diamond Mare" (1, 3) (Active, Graveyard) ["artifact"]
  addCard "Detection Tower" (Active, Graveyard) ["land"]
  addCard "Mox Amber" (Active, Graveyard) ["artifact"]

  addCards 3 "Memorial to Folly" (Active, Play) ["land"]
  addCards 4 "Watery Grave" (Active, Play) ["land"]
  addCards 4 "Overgrown Tomb" (Active, Play) ["land"]

  addCard "March of the Drowned" (Active, Hand) ["sorcery", "black"]
  addCard "Gruesome Menagerie" (Active, Hand) ["sorcery", "black"]
  addCard "Dead Weight" (Active, Hand) ["aura", "black"]
  addCard "Find" (Active, Hand) ["sorcery", "black"]
  addCreature "Vicious Conquistador" (1, 2) (Active, Graveyard) ["black"]
  addCreature "Sailor of Means" (1, 4) (Active, Graveyard) []

  -- This solutions relies on triggering Diamond Mare to gain life, which in
  -- turns triggers Epicure of Blood to cause the opponent to lose life. This
  -- helper can wrap cast actions with that combination.
  let withTriggers = \action name -> do
        action name
        trigger "Diamond Mare"
        c <- requireCard name mempty

        when (hasAttribute "black" c) $ do
          gainLife Active 1
          trigger "Epicure of Blood"
          loseLife Opponent 1
    

  -- Helper function to keep track of which permanent types have been cast
  -- using Muldrotha's ability.
  let castWithMuldrotha = \ptype cn -> do
        let ptypes = S.fromList ["artifact", "creature", "land", "enchantment"]
        let counterName = "muldrotha-" <> ptype

        when (not $ S.member ptype ptypes) $
          throwError $ "Invalid permanent type: " <> ptype

        n <- use (counters . at counterName . non 0)

        if n > 0 then
          throwError $ "Already cast card of type with Muldrotha: " <> ptype
        else
          do
            castFromLocation cn (Active, Graveyard)
            resolve cn
            assign (counters . at counterName) (Just 1)

  step "Detection Tower, Mox Amber, Diamond Mare from graveyard" $ do
    castWithMuldrotha "land" "Detection Tower"
    castWithMuldrotha "artifact" "Mox Amber"
    tap "Detection Tower"
    tap "Mox Amber"
    castWithMuldrotha "creature" "Diamond Mare"

  step "March of the Drowned on Vicious Conquistador" $ do
    tap "Memorial to Folly 1"
    withTriggers cast "March of the Drowned"
    resolve "March of the Drowned"
    returnToHand "Vicious Conquistador"

  step "Vicious Conquistador" $ do
    tap "Memorial to Folly 2"
    withTriggers cast "Vicious Conquistador"
    resolve "Vicious Conquistador"

  step "Dead Weight on Vicious Conquistador" $ do
    tap "Memorial to Folly 3"
    withTriggers cast "Dead Weight"
    target "Vicious Conquistador"
    resolve "Dead Weight"
    modifyStrength "Vicious Conquistador" (-2, -2)
    resetStrength "Vicious Conquistador" (1, 2)
    moveToGraveyard "Dead Weight"

  step "Gruesome Menagerie for Sailor of Means and Vicious Conquistador" $ do
    tap "Watery Grave 1"
    tap "Watery Grave 2"
    tap "Watery Grave 3"
    tap "Watery Grave 4"
    tap "Overgrown Tomb 1"
    withTriggers cast "Gruesome Menagerie"
    resolve "Gruesome Menagerie"
    targetInLocation "Vicious Conquistador" (Active, Graveyard)
    targetInLocation "Sailor of Means" (Active, Graveyard)
    returnToPlay "Vicious Conquistador"
    returnToPlay "Sailor of Means"
    addCard "Treasure" (Active, Play) ["artifact"]
    
  step "Dead Weight on Vicious Conquistador" $ do
    tap "Overgrown Tomb 2"
    withTriggers (castWithMuldrotha "enchantment") "Dead Weight"
    target "Vicious Conquistador"
    modifyStrength "Vicious Conquistador" (-2, -2)
    resetStrength "Vicious Conquistador" (1, 2)
    moveToGraveyard "Dead Weight"

  step "Find for Vicous Conquistador" $ do
    tap "Overgrown Tomb 3"
    tap "Overgrown Tomb 4"
    withTriggers cast "Find"
    resolve "Find"
    targetInLocation "Vicious Conquistador" (Active, Graveyard)
    returnToHand "Vicious Conquistador"

  step "Vicious Conquistador" $ do
    tap "Treasure"
    withTriggers cast "Vicious Conquistador"
    resolve "Vicious Conquistador"

    
guilds_of_ravnica_9 = do
  setLife Opponent 12

  -- Hand
  addCard "Undercity Uprising" (Active, Hand) ["sorcery"]
  addCard "Doublecast" (Active, Hand) ["sorcery"]
  addCard "Plummet" (Active, Hand) ["instant"]
  addCard "Quasiduplicate" (Active, Hand) ["sorcery"]
  addCreature "Torgaar, Famine Incarnate" (7, 6) (Active, Hand) ["creature"]

  -- Play
  addCard "Thousand-Year Storm" (Active, Play) ["enchantment"]
  -- Has +2/+2 from Maniacal Rage aura
  addCreature "Adeliz, the Cinder Wind" (4, 4) (Active, Play) ["flying"]
  addCreature "Afzocan Archer" (1, 4) (Active, Play) []
  addCards 4 "Timber Gorge" (Active, Play) ["land"]
  addCards 4 "Submerged Boneyard" (Active, Play) ["land"]
  addCards 4 "Highland Lake" (Active, Play) ["land"]

  -- Opponent
  addCreature "Kopala, Warden of Waves" (2, 2) (Opponent, Play) ["merfolk"]
  addCreature "Angel 1" (4, 4) (Opponent, Play) ["angel", "flying", "token"]
  addCreature "Angel 2" (4, 4) (Opponent, Play) ["angel", "flying", "token"]
  addCreature "Angel 3" (4, 4) (Opponent, Play) ["angel", "flying", "token"]

  let cn = "Shalai, Voice of Plenty" in
    do
      addCreature cn (3, 4) (Opponent, Play) ["angel", "flying"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "creature")
        (attributeEffect "hexproof")

  let cn = "Lyra Dawnbringer" in
    do
      addCreature cn (5, 5) (Opponent, Play) ["angel", "flying", "lifelink"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "angel")
        (attributeEffect "lifelink" <> strengthEffect (1, 1))

  let cn = numbered 1 "Merfolk Mistbinder" in
    do
      addCreature cn (2, 2) (Opponent, Play) ["merfolk"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "merfolk")
        (strengthEffect (1, 1))

  let cn = numbered 2 "Merfolk Mistbinder" in
    do
      -- Has a +1/+1 counter
      addCreature cn (3, 3) (Opponent, Play) ["merfolk"]
      addEffect cn
        (requireOther cn
          <> requireLocation (Opponent, Play)
          <> requireAttribute "merfolk")
        (strengthEffect (1, 1))
  applyEffect "Shalai, Voice of Plenty"
  applyEffect "Lyra Dawnbringer"
  applyEffect "Merfolk Mistbinder 1"
  applyEffect "Merfolk Mistbinder 2"
  -- This puzzle relies heavily on casting triggers, so wrap the relevant ones
  -- up in this helper.
  let withTriggers = \action name -> do
        action name
        trigger "Thousand-Year Storm"
        storm $ const (copy name)

        trigger "Adeliz, the Cinder Wind"
        modifyStrength "Adeliz, the Cinder Wind" (1, 1)
    
  step "Use Undercity Uprising on Adeliz to destroy Shalai" $ do
    tap $ numbered 1 "Timber Gorge"
    tap $ numbered 1 "Submerged Boneyard"
    tap $ numbered 2 "Submerged Boneyard"

    withTriggers cast "Undercity Uprising"
    resolve "Undercity Uprising"
    forCards
      (requireAttribute "creature" <> requireLocation (Active, Play))
      (setAttribute "deathtouch")

    with "Shalai, Voice of Plenty" $ \enemy -> do
      fight "Adeliz, the Cinder Wind" enemy
      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

  step "Cast Doublecast" $ do
    tap $ numbered 2 "Timber Gorge"
    tap $ numbered 3 "Timber Gorge"
    withTriggers cast "Doublecast"

    resolve "Doublecast"
    resolve "Doublecast"

  step "Cast Plummet to destroy all fliers" $ do
    tap "Timber Gorge 4"
    withTriggers cast "Plummet"

    -- From double doublecast earlier
    copy "Plummet"
    copy "Plummet"

    resolve "Plummet"
    with "Lyra Dawnbringer" $ \enemy -> do
      target enemy
      destroy enemy
      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

    forM_ [1..3] $ \n -> do
      resolve "Plummet"
      with (numbered n "Angel") $ \enemy -> do
        target enemy
        destroy enemy
        validateRemoved enemy

    resolve "Plummet" -- No target

  step "Quasiduplicate on archer, destroy one of the Mistbinders" $ do
    tap $ numbered 1 "Highland Lake"
    tap $ numbered 2 "Highland Lake"
    withTriggers cast "Quasiduplicate"

    with (numbered 2 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..4] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve "Quasiduplicate"
        createToken tokenName (1, 4) (Active, Play)
        fight tokenName enemy

      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

  step "Jump-start Quasiduplicate again (w/ Waterknot), destroy merfolk" $ do
    tap $ numbered 3 "Highland Lake"
    tap $ numbered 4 "Highland Lake"
    withTriggers (jumpstart "Waterknot") "Quasiduplicate"

    with (numbered 1 "Merfolk Mistbinder") $ \enemy -> do
      forM_ [1..2] $ \n -> do
        let tokenName = ("Afzocan Archer " <> show n)
        resolve "Quasiduplicate"
        createToken tokenName (1, 4) (Active, Play)
        fight tokenName enemy

      validate enemy $ requireLocation (Opponent, Graveyard)
      removeEffect enemy

    with "Kopala, Warden of Waves" $ \enemy -> do
      forM_ [3..4] $ \n -> do
        let tokenName = numbered n "Afzocan Archer"
        resolve "Quasiduplicate"
        createToken tokenName (1, 4) (Active, Play)
        fight tokenName enemy

      validate enemy $ requireLocation (Opponent, Graveyard)

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
