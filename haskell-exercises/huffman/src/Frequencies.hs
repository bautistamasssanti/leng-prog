module Frequencies  (Frequency, frequencyMap, frequencies, insert, insertionSort) where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Tuple(swap)
import System.Environment (getArgs)
import Data.List (sortBy)
import Data.Ord (comparing)

type Frequency = (Int, Char)

frequencies::String -> [Frequency]
frequencies str =
  let
  --función (\acc char -> Map.insertWith (+) char 1 acc)
  --valor inicial Map.empty
  --lista str
    freqMap = foldl(\acc char -> Map.insertWith (+) char 1 acc) Map.empty str
    freqList = map swap (Map.toList freqMap)
  in
    insertionSort freqList

frequencyMap::(Ord a) => [a] -> Map a Int
frequencyMap list =
  let
    freqMap = foldl(\acc value -> Map.insertWith (+) value 1 acc) Map.empty list
  in
    freqMap

insert::(Ord a) => a -> [a] -> [a]
insert x [] = x:[] -- Case of empty list
insert x (y:ys)
  | x < y = x:y:ys -- Case in which x < y, insert x at the front
  | x == y = x:y:ys -- Case in which x == y, insert x at the front
  | x > y = y: insert x ys
  -- Case in which x > y, insert y at front and recall the function with x and ys

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort(x:xs) = insert x (insertionSort xs)

main::IO()
main = do
  args <- getArgs
  if null args
    then putStrLn "Error: you didn't specify the file name"
    else do
      let fileName = head args
      content <- readFile fileName
      let freqList = frequencies content
      let sortedFreq = reverse $ sortBy (comparing fst) freqList
      --mapM_ to apply function to all elements without returning the map list
      -- function \ (freq, char) -> putStrLn (show char ++ ": " ++ show freq)
      --list to map sortedFreq
      mapM_ (\ (freq, char) -> putStrLn (show char ++ ": " ++ show freq)) sortedFreq