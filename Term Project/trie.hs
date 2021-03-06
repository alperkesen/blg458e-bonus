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

data Action = Add | Search | Find | Print | Undefined
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


insertList :: [Word] -> Trie
insertList = foldl (\x y -> insert y x) empty


search :: Word -> Trie -> Bool
search [] _ = False
search w@(w':ws') t
  | subtree == empty         = False
  | end subtree && null ws'  = True
  | otherwise                = search ws' subtree
  where
    subtree = (fromMaybe empty . M.lookup w') $ children t


getWords :: Trie -> [Word]
getWords t = getWords' t []
  where
    getWords' :: Trie -> [Char] -> [Word]
    getWords' t acc
      | children t == M.empty = [acc]
      | end t                 = [acc] ++ nextWords
      | otherwise             = nextWords
      where
        lst = M.toList $ children t
        nextWords = concat [getWords' (snd x) (acc ++ [fst x]) | x <- lst]


prefix :: Word -> Trie -> Maybe [Word]
prefix [] trie = Just $ getWords trie
prefix word trie = prefix' word trie
  where
    prefix' :: Word -> Trie -> Maybe [Word]
    prefix' [] t = Just (map (word++) $ getWords t)
    prefix' w@(w':ws') t
      | subtree == empty  = Nothing
      | otherwise         = prefix' ws' subtree
      where
        subtree = (fromMaybe empty . M.lookup w') $ children t


convertAction :: String -> Action
convertAction s = case s of
  "a" -> Add
  "s" -> Search
  "f" -> Find
  "p" -> Print
  _   -> Undefined


doAction :: Trie -> Action -> Word -> IO Trie
doAction trie action word = case action of
  Add -> do
    let newTrie = insert word trie
    putStrLn "New word is added!\n"

    return newTrie

  Search -> do
    let doesExist = search word trie

    if doesExist == True
      then do
        putStrLn "Exists in dictionary!\n"
        return trie
      else do
        putStrLn "NOT exist!\n"
        return trie

  Find -> do
    let words = prefix word trie

    case words of
      Nothing -> do
        putStrLn "No words found with that prefix!\n"
        return trie
      Just x -> do
        putStrLn "Found words:"
        putStrLn (unlines x)
        return trie

  Print -> do
    putStrLn "List of words in dictionary:"
    let words = (unlines . getWords) trie
    putStrLn words
    return trie

  Undefined -> do
    putStrLn "Undefined action!\n"
    return trie


getInput :: Trie -> IO ()
getInput t = do
    putStrLn "a) Add Word"
    putStrLn "s) Search Word"
    putStrLn "f) Find words with prefix"
    putStrLn "p) Print all words"
    putStrLn "e) Exit"
    putStrLn "Enter the action:"
    inp <- getLine

    if inp == "e"
        then return ()
        else do
          let action = convertAction inp

          if action == Print || action == Undefined
            then do
                newT <- doAction t action ""
                getInput newT
            else do
                putStrLn "Enter word/prefix:"
                prefix <- getLine

                newT <- doAction t action prefix
                getInput newT


main :: IO ()
main = do
    args <- getArgs
    wordsFile <- readFile $ head args

    let words = lines wordsFile
    let trie = insertList words

    getInput trie
