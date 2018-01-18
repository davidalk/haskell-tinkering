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

-- Starting out exercises

penultimate l = last (init l)

findK k l = l !! k

isPalindrome l = l == reverse l

duplicate xs = concat [ [x, x] | x <- xs ]

ziplike xs ys = [ (xs !! i, ys !! i) | i <- [0 .. min (length xs) (length ys) - 1]]

splitAtIndex k l = (take k l, drop k l)

dropK k l = take k l ++ drop (k +1) l

slice i k l = take (k-i) (drop i l)