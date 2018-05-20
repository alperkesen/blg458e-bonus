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

type Word = String


empty :: Trie
empty = Trie {end = False, children = M.empty}


insert :: Word -> Trie -> Trie
insert [] t = t
insert [w'] t = Trie {end = end t, children = newChildren}
    where
      subtree = (fromMaybe empty . M.lookup w') $ children t
      endNode = Trie {end = True, children = children subtree}
      newChildren = M.insert w' endNode $ children t
insert ws@(w':ws') t = Trie {end = end t, children = newChildren}
    where
      subtree = (fromMaybe empty . M.lookup w') $ children t
      nextNode = insert ws' subtree
      newChildren = M.insert w' nextNode $ children t