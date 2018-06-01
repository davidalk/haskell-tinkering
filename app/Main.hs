module Main where

import Lib

main :: IO ()
main = someFunc

list = [x * 2 | x <- [1..10], x `mod` 2 == 0]

sumList = sum [5, 67, 10, 100, 12]

countEl x = length (filter (==x) [5, 67, 10, 100, 12, 1, 1, 2, 2, 2, 3, 3, 3, 9, 9, 10])

boomBang xs = [ if x < 50 then "BOOM!" else "BANG!" | x <- xs, odd x ]

multiComp = [ x * y | x <- [1, 2, 3], y <- [1, 2]]

nounAdj = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]
  where
  nouns = ["hobo","frog","pope"]
  adjectives = ["lazy","grouchy","scheming"]

rightAngleTriangles = [ (a, b, c) | c <- [1..10], b <- [1..10], a <- [1..10], a^2 + b^2 == c^2, a+b+c == 24 ]

-- 02 Starting out exercises

penultimate l = last (init l)

findK k l = l !! k

isPalindrome l = l == reverse l

duplicate xs = concat [ [x, x] | x <- xs ]

ziplike xs ys = [ (xs !! i, ys !! i) | i <- [0 .. min (length xs) (length ys) - 1]]

splitAtIndex k l = (take k l, drop k l)

dropK k l = take k l ++ drop (k +1) l

slice i k l = take (k-i) (drop i l)

insertElem x k l = take k l ++ x:(drop k l)

rotate n l = drop n l ++ take n l

-- 03 types and typeclasses

data Colour = Red | Orange | Yellow | Green | Blue | Indigo | Violet
    deriving (Eq, Ord, Show, Bounded, Enum)

firstColour = minBound :: Colour

reverseColourOrder = reverse [minBound .. maxBound] :: [Colour]

paintMix c1 c2 = [fst orderedPair .. snd orderedPair] !! quot (length [fst orderedPair .. snd orderedPair]) 2
  where
  orderedPair = if c1 < c2 then (c1, c2) else (c2, c1)

  -- 04 Syntax in functions

  -- 05 Recursion

stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign a b = -((abs a) + b)

-- 06 Higher order functions
sumInts :: Int -> Int -> Int
sumInts a b = if a < b then a + sumInts (a+1) b else a

sq :: Int -> Int
sq x = x * x

sumSquares :: Int -> Int -> Int
sumSquares a b = if a < b then sq a + sumInts (a+1) b else sq a