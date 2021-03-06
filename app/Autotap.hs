{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

-- An algorithm for figuring out which mana sources to tap to pay for a spell
-- in Magic: The Gathering. Goals are to prefer failure or partial success when
-- unsure of best solution.
--
-- A "perfect" implementation would be very complicated, since it would need to
-- take into account unknown information (think: leaving red mana open in the
-- hope of hitting a Shock off a draw effect.) This implementation aims only to
-- make the best decision given known information, i.e. finding a tapping
-- combination that allows for a unique superset of remaining abilities to be
-- played. In the case of ambiguity (should we leave up Blue or Red mana,
-- having spells for each?) a partial result is returned.
--
-- (Note: this documentation describes desired state, not current)
--
-- TODO:
-- * Consider other abilities in hand/on board.
-- * Consider mana abilities that produce more than one mana.
-- * Consider restricted mana ("only spend this on non-creature spells")
-- * Use a more efficient data structure than [], particularly considering
--   index operations. (probably `Seq`)
module Main where

import Data.List
import Data.Hashable
import GHC.Generics
import qualified Data.Tuple
import qualified Data.HashMap.Strict as M
import Data.Maybe (catMaybes, isNothing, fromJust)
import Data.Monoid ((<>))
import Control.Monad (msum)
import Text.Parsec( runParser, char, (<|>), ParseError, many, many1, digit, eof, choice)

data Restriction a = Any | Specific a deriving (Show)

data Color = Red | Green | White | Black | Blue deriving (Eq, Generic)
--data SpellType = Creature | Instant | Sorcery | Enchantment | Artifact

instance Hashable Color

data ManaSpec = ManaSpec (Restriction Color) deriving (Show)

data ManaPool = ManaPool [Maybe Color]

instance Show ManaPool where
  show (ManaPool cs) = (showColorless . length . filter isNothing $ cs) <> (msum . map show . catMaybes $ cs)
    where
      showColorless 0 = ""
      showColorless n = show n

colorChars =
  [ ('R', Red)
  , ('U', Blue)
  , ('B', Black)
  , ('G', Green)
  , ('W', White)
  ]

inverseColorChars = M.fromList . map Data.Tuple.swap $ colorChars

instance Show Color where
  show color = [Data.Maybe.fromJust . M.lookup color $ inverseColorChars]

data ManaAbility = ManaAbility ManaPool deriving (Show)

data Card = Card
  { cost :: Cost
  , manaAbilities :: [ManaAbility]
  } deriving (Show)

class ManaSource a where
  -- Can the mana source be used to pay for the given spec?
  satisfy :: ManaSpec -> a -> Bool

  -- Given two mana sources that can satify a spec, with no other information,
  -- the one with the lower versatility will be used. For example, a basic land
  -- should be tapped in preference to a dual one. This is only useful to get
  -- better taps in preparation for unknown information (such as draws) - any
  -- known information such as cards in hand will already have been taken into
  -- account.
  --
  -- TODO: Should this be configurable? There's probably some corner cases
  -- where tapping a dual land rather than a basic is the right move?
  versatility :: a -> Int

match :: ManaSpec -> ManaAbility -> Bool
match (ManaSpec (Specific color)) (ManaAbility (ManaPool cs)) = any (\x -> Just color == x) cs
match (ManaSpec Any) (ManaAbility (ManaPool cs)) = not . null $ cs

instance ManaSource Card where
  satisfy spec card = any (match spec) (manaAbilities card)
  versatility card = length $ manaAbilities card

type Cost = [ManaSpec]

mkLand color = mkDualLand [color]
mkDualLand colors = Card { cost = mempty, manaAbilities = [ManaAbility $ ManaPool (map Just colors)] }

deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs


solve :: ManaSource a => Cost -> [Cost] -> [a] -> Maybe [a]
solve cost futureCosts sources = solve' [] cost futureCosts (sortOn versatility sources)

solve' :: ManaSource a => [a] -> Cost -> [Cost] -> [a] -> Maybe [a]
solve' result [] futureCosts _ = Just result
solve' result (cost:rest) futureCosts sources =
  let possibleSources = findIndices (satisfy cost) sources in
  let solves = map (\i -> solve' (sources !! i : result) rest futureCosts (deleteAt i sources)) possibleSources in

  msum solves

parseManaString :: String -> Either ParseError ([Maybe Color])
parseManaString input = runParser manaExpr () input input

manaExpr = do
  result <- many manaChar
  _ <- eof

  return (mconcat result)

manaChar =
      choice (map parseColor colorChars)
  <|> (many1 digit >>= (\n -> return $ replicate (read n) Nothing))
  where
    parseColor (c, color) = (char c >> return [Just color])

mkCost input = case parseManaString input of
                 Right colors -> map toRestriction colors
                 Left err -> error (show err)
 where
   toRestriction (Just x) = ManaSpec (Specific Red)
   toRestriction Nothing = ManaSpec Any

mkSource inputs = case sequence $ map parseManaString inputs of
                     Right colors -> Card { cost = mempty, manaAbilities = map (ManaAbility . ManaPool) colors }
                     Left err -> error (show err)
main = do
  let cost = mkCost "1R"
  let futureCosts = [[ManaSpec (Specific Blue)]]
  let sources =
                [ mkSource ["R"]
                , mkSource ["1"]
                , mkSource ["U", "G"]
                , mkSource ["R", "W"]
                ]

  putStrLn . show . fmap (map manaAbilities) $ solve cost futureCosts sources
