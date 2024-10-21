module Huffman  (huffmanTrie, encode, decode, Trie(..), Bit(..)) where

import qualified Data.Map as M
import Frequencies (Frequency, frequencyMap, insertionSort, insert, frequencies)

data Bit = F | T deriving (Eq, Show)
type Bits = [Bit]  

data Trie a = Empty
            | Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

huffmanTrie::String -> Trie Char
huffmanTrie input =  huffmanTrieHelper (frequencies input) Empty 0

--just to remember it comes sorted from the lesser value to the higher from frequencies
--list of Frequencies, current state trie, current value of the weight
huffmanTrieHelper:: [Frequency] -> Trie Char -> Int-> Trie Char
--initial case of just one element on the trie
huffmanTrieHelper ((_, char): []) Empty _ = (Leaf char) :-: Empty
--initial case of just two elements on the trie
huffmanTrieHelper ((_, char1):(_, char2):[]) Empty _ = (Leaf char1) :-: (Leaf char2)
--initial case
huffmanTrieHelper ((count1, char1):(count2, char2):xs) Empty _ =
  huffmanTrieHelper xs ((Leaf char1) :-: (Leaf char2)) (count1 + count2)
--case of none element left to add
huffmanTrieHelper [] trie _ = trie
--case of one element left to add
huffmanTrieHelper ((count, char):[]) trie currentWeight
  | count <= currentWeight = (Leaf char) :-: trie
  | otherwise = trie :-: (Leaf char)
--case of n elements left to add
huffmanTrieHelper ((count, char):xs) trie currentWeight
  | count <= currentWeight = huffmanTrieHelper xs ((Leaf char) :-: trie) (count + currentWeight)
  | otherwise = huffmanTrieHelper xs (trie :-: (Leaf char)) (count + currentWeight)

encode :: String -> Trie Char -> Bits
encode input code = error "Implement it"

trieToList :: Trie a -> [Int] -> [(a, Bits)]
--handling the case of empty, which only arrives if there is just one element
trieToList Empty _ = []
--case when you arrived to a leaf
trieToList (Leaf value) bits = [(value, bits)]
--general case
trieToList (left :-: right) bits = trieToList left (bits ++ [F]) ++ trieToList right (bits ++ [T])

decode::Bits -> Trie Char -> String
decode bits trie = error "Implement it"