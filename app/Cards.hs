module Cards where

data GameState = GSBidding | GSTricking | GSFinished
data Bid = Bid Int (Maybe Suit) | Misere | OpenMisere
type Player = Int

data Rank = Ace | King | Queen | Jack | RankN Int
  deriving (Eq, Ord)

instance Show Rank where
  show Ace = "A"
  show King = "K"
  show Queen = "Q"
  show Jack = "J"
  show (RankN n) = show n

allRanks = map RankN [3..10] ++ [Jack, Queen, King, Ace]

data Suit = Spade | Club | Diamond | Heart
  deriving (Eq)

instance Show Suit where
  show Spade = "♠"
  show Club = "♣"
  show Diamond = "♦"
  show Heart = "♥"

allSuits :: [Suit]
allSuits = [Spade, Club, Diamond, Heart]

data Card = Joker | Card Suit Rank

instance Show Card where
  show Joker = "🃟"
  show (Card s r) = show r ++ show s

data CardWithTrump = CardWithTrump Card (Maybe Suit)

sortableCard :: Maybe Suit -> Card -> Card -> (Int, Card)
sortableCard None led Joker = (0, card)
sortableCard None led c@(Card _ rank) = (rank, card)

instance Ord CardWithTrump where
  compare (CardWithTrump c1 t1) (CardWithTrump c2 t2) = 

  case t1 of
    None -> c1
  ((Joker, None), (Jack, Just t1), (Jack, Just (suitComplement t1), c1)

data Trick = [Card]
data Round = Round Player Bid [Trick]

data FiveHundred = FiveHundred
  { _fhState :: GameState
  , _fhBid :: Maybe (Player, Bid)
  , _fhCurrentDealer :: Player
  , _fhCurrentPlayer :: Player
  , _fhHands :: Vec [Card]
  , _fhKitty :: [Card]
  , _fhTricks :: [Trick]
  , _fhRounds :: [Round]
  }

data Action =
    ActionBid (Maybe Bid)
  | ActionPlay Card
deck :: [Card]
deck = Joker :
  [ Card s r
  | s <- allSuits
  , r <- allRanks
  , RankN (if isBlack s then 4 else 3) <= r
  ]

main = putStrLn deck

