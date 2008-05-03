module Main where

import Foreign
import Data.Char
import Data.List

-- Define some card types so that we can compare cards naturally

data Game = Win | Loss | Tie
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord)
data Card = Card Int Suit deriving (Eq)
type Hand = [Card]
type Set  = [Card] -- This is something like a flush or a full house
type HandSet = (Hand, Set)
data HandRank = HighCard | OnePair | TwoPair | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush deriving (Eq, Ord)

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

getSuit :: Card -> Suit
getSuit (Card _ s) = s

getCardInt :: Card -> Int
getCardInt (Card i _) = i

-- tests whether a hand is a flush
isFlush :: Hand -> Bool
isFlush (c:cs) = all (== (getSuit c)) (map getSuit cs)

isStraight :: Hand -> Bool
isStraight h = isStraight' h
    where
    isStraight' []  = True
    isStraight' [x] = True
    isStraight' (c:c':cs) = (c == (c' - 1)) && (isStraight' (c':cs))

allSameInList :: Eq a => [a] -> Bool
allSameInList [] = True
allSameInList (c:cs) = all (== c) cs


countSortedList :: Ord a => [a] -> [(a, Int)]
countSortedList []     = []
countSortedList (x:xs) = (x, ct) : (countSortedList rm)
    where
    consume x n (y:ys) = if y == x
                            then consume x (n+1) ys 
                            else (n, y:ys)
    (ct, rm) = consume x 1 xs

countList :: Ord a => [a] -> [(a, Int)]
countList = countSortedList . sort

getHandRank :: Hand -> HandRank
getHandRank h
    | (isFlush h) && ((sort $ map getCardInt h) == [1, 10, 11, 12, 13])
      = RoyalFlush
    | (isFlush h) && (isStraight h) 
      = StraightFlush
    | (allSameInList $ init svals) || (allSameInList $ tail svals) 
      = FourKind
    | (length $ nub $ vals) == 2
      = FullHouse
    | isFlush h
      = Flush
    | isStraight h
      = Straight
    where
    vals = map getCardInt h
    svals = sort vals

pokerHands :: [(HandSet, HandSet)]
pokerHands = map lineToHands (lines pokerTxt)
    where
    pokerTxt = unsafePerformIO (readFile "poker.txt")

a :: Card
a = cardRead "4H"

b = cardRead "TD"

main = print pokerHands
