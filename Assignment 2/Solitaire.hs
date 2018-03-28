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
  Spades -> Black
  Clubs -> Black
  Diamonds -> Red
  Hearts -> Red

