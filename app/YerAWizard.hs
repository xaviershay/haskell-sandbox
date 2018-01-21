{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings   #-}

-- Playing around with modelling the Hogwarts Battle board game
module Main where

import           Control.Lens  hiding (Const)
import           Control.Monad (forM_)
import           Data.List     (intercalate)
import           Data.Maybe    (catMaybes)
import           Data.Monoid
import qualified Data.Text     as T
import Text.Printf (printf)

data ResourcePool = ResourcePool {
    _influence :: Int
  , _power :: Int
  , _health :: Int
} deriving (Show)
makeLenses ''ResourcePool

instance Monoid ResourcePool where
  mempty = ResourcePool { _influence = 0, _power = 0, _health = 0 }
  mappend x ResourcePool { _influence = i, _power = p, _health = h } =
    x & influence +~ i & power +~ p & health +~ h

data Reward =
    GainResources ResourcePool
  | GrantToOne Reward
  | GrantToAll Reward
  | DrawCard Int
  | Multiple [Reward]
  | Choose Int [Reward]

instance Show Reward where
  show (GrantToAll x) = "ALL Heroes " <> show x
  show (GrantToOne x) = "any one hero " <> show x
  show (DrawCard 1) = "draw a card"
  show (DrawCard n) = "draw " <> show n <> " cards"
  show (Choose 1 rs) = intercalate "; or " . map show $ rs
  show (Choose n rs) = "Choose " <> show n <> ": " <> (intercalate "; " . map show $ rs)
  show (Multiple rs) = intercalate "; and " . map show $ rs
  show (GainResources pool) = intercalate ", " . catMaybes $
    [ f "gain $" influence
    , f "gain ⚔" power
    , f "gain ❤" health
    ]

    where
      f symbol selector = case pool ^. selector of
                            0 -> Nothing
                            x -> Just $ symbol <> show x

gain x    = GainResources $ mempty & x
oneHero   = GrantToOne
allHeroes = GrantToAll
draw      = DrawCard
choose    = Choose 1
combine   = Multiple

data CardType = Spell | Item | Ally deriving (Show, Eq)

data Card = Card {
    _name :: String
  , _cardType :: CardType
  , _cost :: Int
  , _reward :: Reward
  -- TODO: How to handle discard effects?
}
makeLenses ''Card

instance Show Card where
  show x = printf "%s (%s/%d): %s"
            (x ^. name)
            (show $ x ^. cardType)
            (x ^. cost)
            (show $ x ^. reward)

mkCard name ctype cost reward = Card { _name = name, _cardType = ctype, _cost = cost, _reward = reward }

cards =
  [ mkCard "Alohomora" Spell 0 $
      gain $ influence +~ 1

  , mkCard "Mandrake" Item 0 $
      choose [ gain $ power +~ 1 , oneHero (gain $ health +~ 2) ]

  , mkCard "Reparo" Spell 3 $
      choose [ gain $ influence +~ 2 , draw 1 ]

  , mkCard "Hargid" Ally 3 $
      combine [ gain $ power +~ 1, allHeroes $ draw 1 ]
  ]

main :: IO ()
main = do
  forM_ cards print
  putStrLn ""
