module Main where

import Foreign
import Data.Char
import Data.List

-- Define some card types so that we can compare cards naturally

data Game = Win | Loss | Tie
data Suit = Hearts | Clubs | Diamonds | Spades deriving (Eq, Ord, Show)
data Card = Card Int Suit deriving (Eq, Show)
type Hand = [Card]
type Set  = [Card] -- This is something like a flush or a full house
type HandSet = (Hand, Set)
data HandRank = HighCard | OnePair | TwoPair | ThreeKind | Straight | Flush | FullHouse | FourKind | StraightFlush | RoyalFlush deriving (Eq, Ord, Show)

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
isStraight h = isStraight'$ map getCardInt h
    where
    isStraight' []  = True
    isStraight' [x] = True
    isStraight' (c:c':cs) = (c == (c' - 1)) && (isStraight' (c':cs))

-- Turn a list into a list of (item, how many times it appeared in the list)
countSortedList :: Ord a => [a] -> [(a, Int)]
countSortedList []     = []
countSortedList (x:xs) = (x, ct) : (countSortedList rm)
    where
    consume x n []     = (n, [])
    consume x n (y:ys) = if y == x
                            then consume x (n+1) ys 
                            else (n, y:ys)
    (ct, rm) = consume x 1 xs

countList :: Ord a => [a] -> [(a, Int)]
countList = countSortedList . sort

getHandRank :: Hand -> HandRank
getHandRank h
    | flush_p && ((sort $ map getCardInt h) == [1, 10, 11, 12, 13])
      = RoyalFlush
    | flush_p && straight_p
      = StraightFlush
    | any (== 4) (map snd counted)
      = FourKind
    | (length $ nub $ vals) == 2
      = FullHouse
    | flush_p
      = Flush
    | straight_p
      = Straight
    | any (== 3) (map snd counted)
      = ThreeKind
    | sort (map snd counted) == [1,2,2]
      = TwoPair
    | any (== 3) (map snd counted)
      = OnePair
    | otherwise
      = HighCard
    where
    straight_p = isStraight h
    flush_p = isFlush h
    counted = countList h
    vals = map getCardInt h
    svals = sort vals

pokerHands :: [(Hand, Hand)]
pokerHands = map (lineToHands . init) (lines pokerTxt)
    where
    pokerTxt = unsafePerformIO (readFile "poker.txt")

highCardTieBreaker :: Hand -> Hand -> Bool
highCardTieBreaker h1 h2 = agt
    where
    m1 = maximum $ map getCardInt h1
    m2 = maximum $ map getCardInt h2
    (a1, a2) = (m1, maximum [getSuit c | c <- h1, (getCardInt c == m1)])
    (b1, b2) = (m2, maximum [getSuit c | c <- h2, (getCardInt c == m2)])
    agt = if a1 > b1 then True else (if a1 == b1 then a2 > b2 else False)

--answer :: Int
--main = print [(getHandRank a, getHandRank b) | (a, b) <- pokerHands, getHandRank a /= HighCard]


ph = [([Card 1 Spades, Card 2 Spades, Card 3 Spades, Card 4 Spades, Card 5 Spades], [Card 1 Spades, Card 2 Hearts, Card 3 Spades, Card 4 Hearts, Card 5 Spades])]

--main = print pokerHands
main = print $ 1 + length [(ra, rb) | (a, b) <- pokerHands, let ra = getHandRank a, let rb = getHandRank b, (ra == rb) && (highCardTieBreaker a b)]
--main = print [(getHandRank a, getHandRank b) | (a, b) <- ph]
