-- 
--   BLG 458E - Functional Programming
--   Bonus Assignment 2
--
--   Name: Ekrem Alper Kesen
--   ID:   150150018
--
--


data Color = Red | Black
data Suit = Clubs | Diamonds | Hearts | Spades
data Rank = Num Int | Jack Queen | King | Ace
data Card = Card { suit :: Suit, rank :: Rank }
data Move = Draw | Discard Card
