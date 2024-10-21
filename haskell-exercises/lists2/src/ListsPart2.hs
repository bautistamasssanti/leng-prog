module ListsPart2 (Bit(..), bitAt, charToBits, bits, queens) where

import Data.Char(ord)  
import Data.Bits(testBit)
  
data Bit = F | T  deriving (Eq, Show, Enum, Read)
type Bits = [Bit]

bitAt :: Int -> Char -> Bit
bitAt n c = if testBit (ord c) (7-n) then T else F 

charToBits :: Char -> Bits
charToBits c = [bitAt i c| i <- [0..7]]


bits::String -> Bits
bits str = foldr (++) [] (map charToBits str)
--primero mapeo para aplicar a todo el string char to bits
--despues

type Solution = [(Int, Int)]


queens::Int -> [Solution]
queens n
  | n < 8 = []
  | otherwise queensHelper (0, 0) n []

--xPointer yPointer limit solutions
queensHelper:: (Int, Int) -> Int -> [Solution] -> [Solution]
queensHelper x y limit solution
  | x == n && y == n = solutions
  |

queensSolution:: (Int, Int) -> Int ->

movePosition:: (Int, Int) -> Int -> (Int, Int)
movePosition (x, y) limit
  | y == n = (x + 1, 0)
  | otherwise = (x, y + 1)

meetsConditions::(Int, Int) -> Solution -> Boolean
meetsConditions coord solution = (isNotOccupied coord solution) && (inNotReachable coord solution)

isNotOccupied::(Int, Int) -> Solution -> Boolean
isNotOccupied (x, y) [] = True
isNotOccupied (x, y) ((x1, y1): xs)
  | x == x1 && y == y1 = False
  | otherwise = isNotOccupied (x, y) xs

isNotReachable::(Int, Int) -> Solution -> Boolean
isNotReachable (x , y) [] = True
isNotReachable (x, y) ((x1, y1): xs)
  | x == x1 || y == y1 || (abs (x1 - x) == abs(y1 - y)) =  False
  | otherwise = isNotReachable (x, y) xs