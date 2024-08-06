module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz a
  | a <= 0 = Nothing
  | otherwise = Just(collatzHelper a 0)

collatzHelper :: Integer -> Integer -> Integer
collatzHelper value counter
  | value == 1 = counter
  | value `mod` 2 == 0 = collatzHelper (value `div` 2) (counter + 1)
  | otherwise = collatzHelper (value * 3 + 1) (counter + 1)
