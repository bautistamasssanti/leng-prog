module Trie  (Trie(..), left, right, find, decode, toList) where

import Bit
  
data Trie a = Leaf a
            | Trie a :-: Trie a deriving (Eq, Show, Ord)

-- Bring definition from previous TP

left::Trie a -> Trie a
left (l :-: _) = l
left (Leaf _) = error "Already on leaf"

right::Trie a -> Trie a
right (_ :-: r) = r
right (Leaf _) = error "Already on leaf"
  
find::Bits -> Trie a -> a
find _ (Leaf value) = value
find (bit:bits) trie
  | bit == F = find bits (left trie)
  | otherwise = find bits (right trie)

decode::Bits -> Trie Char -> String
decode bits trie = decodeHelper bits trie trie

--bits list, current trie, original trie to reset when value found
decodeHelper::Bits -> Trie Char -> Trie Char -> String
decodeHelper bits (Leaf value) originalTrie = value : (decodeHelper bits originalTrie originalTrie)
decodeHelper [] _ _ = ""
decodeHelper (bit:bits) currentTrie originalTrie
  | bit == F = decodeHelper bits (left currentTrie) originalTrie
  |otherwise = decodeHelper bits (right currentTrie) originalTrie

toList::Trie a -> [(a, Bits)]
toList trie = toListHelper trie []

toListHelper::Trie a -> Bits -> [(a, Bits)]
toListHelper (Leaf value) bits = [(value, bits)]
toListHelper (l :-: r) bits = toListHelper l (bits ++ [F]) ++ toListHelper r (bits ++ [T])
