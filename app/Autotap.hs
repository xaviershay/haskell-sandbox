module Main where

import Data.List

data Restriction a = Any | Specific a deriving (Show)

data Color = Red | Green | White | Black | Blue deriving (Show, Eq)
--data SpellType = Creature | Instant | Sorcery | Enchantment | Artifact

data ManaSpec = ManaSpec (Restriction Color) deriving (Show)

data ManaPool = ManaPool [Maybe Color] deriving (Show)

data ManaAbility = ManaAbility ManaPool deriving (Show)

data Card = Card
  { cost :: Cost
  , manaAbilities :: [ManaAbility]
  } deriving (Show)

class ManaSource a where
  satisfy :: ManaSpec -> a -> Bool

match :: ManaSpec -> ManaAbility -> Bool
match (ManaSpec (Specific color)) (ManaAbility (ManaPool cs)) = any (\x -> Just color == x) cs
match (ManaSpec Any) (ManaAbility (ManaPool cs)) = not . null $ cs

instance ManaSource Card where
  satisfy spec card = any (match spec) (manaAbilities card)

type Cost = [ManaSpec]

mkLand color = mkDualLand [color]
mkDualLand colors = Card { cost = mempty, manaAbilities = [ManaAbility $ ManaPool (map Just colors)] }

deleteAt idx xs = lft ++ rgt
  where (lft, (_:rgt)) = splitAt idx xs

solve :: ManaSource a => Cost -> [a] -> Maybe [a]
solve = solve' []

solve' :: ManaSource a => [a] -> Cost -> [a] -> Maybe [a]
solve' result [] _ = Just result
solve' result (cost:rest) sources =
  case findIndex (satisfy cost) sources of
    Nothing -> Nothing
    Just index -> solve' (sources !! index : result) rest (deleteAt index sources)

main = do
  let cost = [ManaSpec Any, ManaSpec (Specific Red)]
  let sources =
                [ mkLand Red
                , mkLand White
                , mkDualLand [Blue, Green]
                ]

  putStrLn . show $ solve cost sources
