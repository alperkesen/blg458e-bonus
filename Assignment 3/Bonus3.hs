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


{-
    Takes a sentence (list of words) and returns character counts as a map.

    Example:

      sentenceCharCounts ["you", "now"]

      -> fromList [('n',1),('o',2),('u',1),('w',1),('y',1)]

-}

sentenceCharCounts :: Sentence -> CharCountMap
sentenceCharCounts ws = Map.unionsWith (+) $ map wordCharCounts ws


{-
    Takes a list of words and returns a map from word to character counts.

    Example:

      dictCharCounts ["ate", "eat"]

      -> fromList [("ate",fromList [('a',1),('e',1),('t',1)]),
                   ("eat",fromList [('a',1),('e',1),('t',1)])]

-}

dictCharCounts :: [Word] -> WordCharCountMap
dictCharCounts ws = Map.fromList [(w, ccmap) | w <- ws,
                                  let ccmap = wordCharCounts w]


{-
    Takes a map from word to character counts.
    Returns a map from character counts to list of words.

    Example:

      fromList [("ate",fromList [('a',1),('e',1),('t',1)]),
                ("eat",fromList [('a',1),('e',1),('t',1)])]

      -> [(fromList [('a',1),('e',1),('t',1)],["eat","ate"])]

-}

dictWordsByCharCounts :: WordCharCountMap -> CharCountWordMap
dictWordsByCharCounts wccmap = Map.fromListWith (++) [(snd w, [fst w]) |
                                                      w <- Map.toList wccmap]


{-
    Takes a word, a map from character counts to list of words.
    Returns list of words.

    Example:

      wordAnagrams "tea" [(fromList [('a',1),('e',1),('t',1)],["eat","ate"])]

      -> ["eat","ate"]

-}

wordAnagrams :: Word -> CharCountWordMap -> [Word]
wordAnagrams w ccwmap = case Map.lookup cc ccwmap of
  Nothing -> []
  Just a  -> a
  where
    cc = wordCharCounts w


{-
    Takes a map of character counts.
    Returns a list of maps of character counts.

    Example:

      charCountsSubsets $ fromList [('a',1),('e',1),('t',1)]

      -> [fromList [('a',1),('e',1),('t',1)],
          fromList [('a',1),('e',1)],
          fromList [('a',1),('t',1)],
          fromList [('a',1)],
          fromList [('e',1),('t',1)],
          fromList [('e',1)],
          fromList [('t',1)],
          fromList []]

-}

charCountsSubsets :: CharCountMap -> [CharCountMap]
charCountsSubsets ccm = (nub . map wordCharCounts) $ charCountsSubsets' word
  where
    word = concat [replicate (snd x) (fst x) | x <- Map.toList ccm]
    charCountsSubsets' :: Word -> [Word]
    charCountsSubsets' [] = [[]]
    charCountsSubsets' cs@(c':cs') = map (c':) subsets ++ subsets
      where
        subsets = charCountsSubsets' cs'
