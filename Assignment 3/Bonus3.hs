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
