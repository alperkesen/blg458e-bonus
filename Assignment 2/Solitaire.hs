-- 
--   BLG 458E - Functional Programming
--   Bonus Assignment 2
--
--   Name: Ekrem Alper Kesen
--   ID:   150150018
--
--

module Solitaire where

data Color = Red | Black deriving (Eq, Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)
data Move = Draw | Discard Card deriving (Eq, Show)


cardColor :: Card -> Color
cardColor card = case suit card of
  Spades   -> Black
  Clubs    -> Black
  Diamonds -> Red
  Hearts   -> Red

cardValue :: Card -> Int
cardValue card = case rank card of
  Ace   -> 11
  Num x ->  x
  _     -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard [] c = error "Couldn't find the card"
removeCard cs@(x:xs) c
  | x == c     = xs
  | otherwise  = x : removeCard xs c

allSameColor :: [Card] -> Bool
allSameColor []  = True
allSameColor [_] = True
allSameColor (x:y:xs)
  | cardColor x == cardColor y = allSameColor (y:xs)
  | otherwise                  = False

sumCards :: [Card] -> Int
sumCards cs = sumCards' 0 cs
  where
    sumCards' ::  Int -> [Card] -> Int
    sumCards' acc [] = acc
    sumCards' acc (x:xs) = sumCards' newAcc xs
      where
        newAcc = acc + cardValue x
