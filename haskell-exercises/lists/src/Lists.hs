module Lists (member, union, intersection, difference,
              insert, insertionSort,
              binaryToDecimal, toDecimal, toDec, decimal, firsts,
              binaryAdd, merge) where
  
import Data.Char(digitToInt)  

member:: Int -> [Int] -> Bool
member _ []      = False
member e (x:xs)  = e == x || member e xs


union:: [Int] -> [Int] -> [Int]
union [] ys     = ys
union (x:xs) ys 
  | member x ys = union xs ys
  | otherwise   = x : union xs ys

-- Remove Implementations, from, here on

intersection:: [Int] -> [Int] -> [Int]
intersection _ [] = [] -- Case where the second list is empty
intersection [] _ = [] -- Case where the first list is empty
intersection (x:xs) ys -- regular case, with guards for if
  | elem x ys = x: intersection xs ys
  | otherwise = intersection xs ys
  -- if x is included in ys it gets added to the return, then recursively calls the function without x
  -- otherwise it calls the function without x to go on

difference:: [Int] -> [Int] -> [Int]
difference  _ [] = [] -- Case where the second list is empty
difference [] _ = [] -- Case where the first list is empty
difference (x:xs) ys -- regular case, with guards for if
  | elem x ys = difference xs ys
  | otherwise = x: difference xs ys
  -- if x is included in ys it goes on with the next element
  -- otherwise x is included on the return list, and goes on to get the other elements

insert:: Int -> [Int] -> [Int]
insert x [] = x:[] -- Case of empty list
insert x (y:ys)
  | x < y = x:y:ys -- Case in which x < y, insert x at the front
  | x == y = x:y:ys -- Case in which x == y, insert x at the front
  | x > y = y: insert x ys
  -- Case in which x > y, insert y at front and recall the function with x and ys

insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort(x:xs) = insert x (insertionSort xs)

binaryToDecimal :: [Int] -> Int
binaryToDecimal [] = 0
binaryToDecimal (x:xs) = (x * 2^(length xs)) + (binaryToDecimal xs)
    
toDecimal :: Int -> [Int] -> Int
toDecimal  _ [] = 0
toDecimal x (y:ys) = (y * x^(length ys)) + (binaryToDecimal ys)
    
toDec::Int -> String -> Int
toDec _ [] = 0
toDec x (y:ys) = ((digitToInt y) * x^(length ys)) + (toDec x ys)

-- Same as `toDec` But use a list comprehension
decimal::Int -> String -> Int
decimal base str = sum [(digitToInt val) * base^i | (val, i) <-zip(reverse str) [0..]]
--[ expression | pattern <- list, condition ]

firsts::[a] -> [[a]]
firsts xs = [take n xs | n <- [1..length xs]]
--take n xs devuelve una sublista de los primeros n elementos de la lista xs



-- Given two String that represents numbers in binary implement the 'binaryAdd' function
-- DO NOT USE a predefined '+' operation

binaryAdd::String -> String -> String
binaryAdd x y = binaryAddAux '0' (reverse x) (reverse y)

binaryAddAux::Char -> String -> String -> String
binaryAddAux carry [] [] = if carry == '1' then "1" else []
binaryAddAux carry (x:xs) [] = add carry (x:xs) "0"
binaryAddAux carry [] (y:ys) = add carry "0" (y:ys)
add carry (x:xs) (y:ys) =
  let
    (newCarry,value) = fullAdder carry x y
  in value: (binaryAddAux newCarry xs ys)

fullAdder::Char -> Char -> Char -> (Char, Char) --(carry, value)
fullAdder '0' '0' '0' = ('0','0')
fullAdder '0' '0' '1' = ('0','1')
fullAdder '0' '1' '0' = ('0','1')
fullAdder '1' '0' '0' = ('0','1')
fullAdder '1' '0' '1' = ('1','0')
fullAdder '1' '1' '0' = ('1','0')
fullAdder '0' '1' '1' = ('1','0')
fullAdder '1' '1' '1' = ('1','1')

merge::(Ord a) => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge (a: as) (b: bs)
  | a == b = a:b:(merge as bs)
  | a > b = b:(merge(a:as) bs)
  | otherwise = a:(merge as (b:bs))