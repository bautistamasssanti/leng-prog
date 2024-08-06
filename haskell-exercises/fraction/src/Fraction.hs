module Fraction (Fraction, add, sub, mul, divide, hcf) where

type Fraction = (Int, Int)

-- Implement the `add` Function

add :: Fraction -> Fraction -> Fraction
add (n1, d1) (n2, d2) =
  let
    numerator = n1 * d2 + n2 * d1
    divisor = d1 * d2
    comDiv = hcf numerator divisor
    numHcf = numerator `div` comDiv
    divHcf = divisor `div` comDiv
  in (numHcf, divHcf)

-- Implement the `sub` Function

sub :: Fraction -> Fraction -> Fraction
sub (n1, d1) (n2, d2) =
  let
    numerator = n1 * d2 - n2 * d1
    divisor = d1 * d2
    comDiv = hcf numerator divisor
    numHcf = numerator `div` comDiv
    divHcf = divisor `div` comDiv
  in (numHcf, divHcf)

-- Implement the `mul` Function

mul :: Fraction -> Fraction -> Fraction
mul (n1, d1) (n2, d2) =
  let
    numerator = n1 * n2
    divisor = d1 * d2
    comDiv = hcf numerator divisor
    numHcf = numerator `div` comDiv
    divHcf = divisor `div` comDiv
  in (numHcf, divHcf)

-- Implement the `divide` Function

divide :: Fraction -> Fraction -> Fraction
divide (n1, d1) (n2, d2) =
  let
    numerator = n1 * d2
    divisor = n2 * d1
    comDiv = hcf numerator divisor
    numHcf = numerator `div` comDiv
    divHcf = divisor `div` comDiv
  in (numHcf, divHcf)
  

-- Implement the `hcf` Function

hcf :: Int -> Int -> Int
hcf 0 b = b
hcf a 0 = a
hcf a b = hcf b remainder
  where remainder = a `mod` b



    