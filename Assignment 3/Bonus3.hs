-- 
--   BLG 458E - Functional Programming
--   Bonus Assignment 3
--
--   Name: Ekrem Alper Kesen
--   ID:   150150018
--
--


import Prelude hiding (Word)

import System.Environment
import System.IO

import qualified Data.Map as Map
import Data.List
import Data.List.Split
import Data.Char


type Word = String
type Sentence = [Word]
type CharCounts = [(Char, Int)]
type CharCountMap = Map.Map Char Int
type WordCharCountMap = Map.Map Word CharCountMap
type CharCountWordMap = Map.Map CharCountMap [Word]


{-
    Takes a word and returns character counts as a map.

    Example:

      wordCharCounts "eat"

      -> fromList [('a',1),('e',1),('t',1)]

-}
wordCharCounts :: Word -> CharCountMap
wordCharCounts ws = Map.fromList [(head x, length x) |
                                  x <- groupBy (\x y -> x == y)
                                   $ (sort . (map toLower)) ws]
