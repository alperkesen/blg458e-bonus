-- 
--   BLG 458E - Functional Programming
--   Term Project
--
--   Name: Ekrem Alper Kesen
--   ID:   150150018
--
--


import qualified Data.Map as M
import Data.Maybe

import System.Environment
import System.IO

import Prelude hiding (Word)


data Trie = Trie {end :: Bool, children :: M.Map Char Trie} 
    deriving (Eq, Show)