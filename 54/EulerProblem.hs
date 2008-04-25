module Main where

-- Define some card types so that we can compare cards naturally

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord)
data Card = Card Int Suit deriving (Eq)
type Hand = [Card]

instance Ord Card where
    compare (Card a b) (Card c d)
        | a > c  = GT
        | a < c  = LT
        | a == c = compare b d

-- End card definitions


main = print (compare a b)
