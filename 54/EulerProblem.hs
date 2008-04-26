module Main where

import Foreign
import Data.Char

-- Define some card types so that we can compare cards naturally

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord)
data Card = Card Int Suit deriving (Eq)
type Hand = [Card]

type HandRank = HighCard | OnePair | TwoPair | TheeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush deriving (Eq, Ord)

instance Ord Card where
    compare (Card a b) (Card c d)
        | a > c  = GT
        | a < c  = LT
        | a == c = compare b d

-- kind of a cop out since i couldn't figure out how to make instances of read work
cardRead :: String -> Card
cardRead (x:xs) = Card (num x) (suitRead xs)
    where
    num n
        | n == 'A'            = 1
        | n `elem` "23456789" = digitToInt n
        | n == 'T'            = 10
        | n == 'J'            = 11
        | n == 'Q'            = 12
        | n == 'K'            = 13
    suitRead "H" = Hearts
    suitRead "C" = Clubs
    suitRead "D" = Diamonds
    suitRead "S" = Spades

-- End card definitions

lineToHands :: String -> (Hand, Hand)
lineToHands l = (mcr h1, mcr h2)
    where
    lineWords = words l
    h1 = take 5 lineWords
    h2 = drop 5 lineWords
    mcr = map cardRead

pokerHands :: [(Hand, Hand)]
pokerHands = map lineToHands (lines pokerTxt)
    where
    pokerTxt = unsafePerformIO (readFile "poker.txt")

tieBreaker :: Hand -> Hand -> Bool
tieBreaker 

a :: Card
a = cardRead "4H"

b = cardRead "TD"

main = print pokerHands
