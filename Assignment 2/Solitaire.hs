-- 
--   BLG 458E - Functional Programming
--   Bonus Assignment 2
--
--   Name: Ekrem Alper Kesen
--   ID:   150150018
--
--

import Data.Char

data Color = Red | Black deriving (Eq, Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)
data Move = Draw | Discard { card :: Card } deriving (Eq, Show)
data State = Start | Continue | End deriving (Eq, Show)


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

score :: [Card] -> Int -> Int
score cs g
  | s > g     = if allSameColor cs then div (3 * (s - g)) 2 else 3 * (s - g)
  | otherwise = if allSameColor cs then div (g - s) 2 else g - s
    where
      s = sumCards cs

runGame :: [Card] -> [Move] -> Int -> Int
runGame cs ms g = playGame Start cs ms []
  where
    playGame :: State -> [Card] -> [Move] -> [Card] -> Int
    playGame Start cs ms _  = playGame Continue cs ms []
    playGame End _ _ hs = score hs g
    playGame _ cs [] hs = playGame End cs [] hs
    playGame Continue cards@(c:cs) (m:ms) hs
      | m /= Draw                     = playGame Continue cards ms (removeCard hs dc)
      | m == Draw && cards == []      = playGame End cs ms hs
      | m == Draw && sumCards hs' > g = playGame End cs ms hs'
      | otherwise                     = playGame Continue cs ms hs'
      where
        dc = card m
        hs' = c : hs

convertSuit :: Char -> Suit
convertSuit c
  | c == 'd' || c == 'D' = Diamonds
  | c == 'h' || c == 'H' = Hearts
  | c == 'c' || c == 'C' = Clubs
  | c == 's' || c == 'S' = Spades
  | otherwise            = error "Unknown suit"


convertRank :: Char -> Rank
convertRank c
  | c == 't' || c == 'T' = Num 10
  | c == 'j' || c == 'J' = Jack
  | c == 'q' || c == 'Q' = Queen
  | c == 'k' || c == 'K' = King
  | c == '1'             = Ace
  | isDigit c            = Num (digitToInt c)
  | otherwise            = error "Unknown rank"

convertCard :: Char -> Char -> Card
convertCard s r = Card suit rank
  where
    suit = convertSuit s
    rank = convertRank r

readCards :: IO [Card]
readCards = readCards' []
  where
    readCards' :: [Card] -> IO [Card]
    readCards' cs = do
      line <- getLine
      if line == "."
        then return cs
        else do let s = line !! 0
                let r = line !! 1
                let c = convertCard s r

                readCards' (c:cs)

convertMove :: Char -> Char -> Char -> Move
convertMove 'd' _ _ = Draw
convertMove 'D' _ _ = Draw
convertMove c s r
  | c == 'r' || c == 'R' = Discard (convertCard s r)
  | otherwise            = error "Unknown Move"

readMoves :: IO [Move]
readMoves = readMoves' []
  where
    readMoves' :: [Move] -> IO [Move]
    readMoves' ms = do
      line <- getLine
      if line == "."
        then return ms
        else do let t = line !! 0
                let s = line !! 1
                let r = line !! 2

                let m = convertMove t s r

                readMoves' (m:ms)

main :: IO ()
main = do putStrLn "Enter cards:"
          cards <- readCards
          --putStrLn (show cards)

          putStrLn "Enter moves:"
          moves <- readMoves
          --putStrLn (show moves)

          putStrLn "Enter goal:"
          line <- getLine

          let goal = read line :: Int
          let score = runGame cards moves goal
          putStrLn ("Score: " ++ show score)
