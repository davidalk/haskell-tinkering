module Main where

import Lib
import DistanceConversions

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

englishDigit :: Int -> String
englishDigit 9 = "nine"
englishDigit 8 = "eight"
englishDigit 7 = "seven"
englishDigit 6 = "six"
englishDigit 5 = "five"
englishDigit 4 = "four"
englishDigit 3 = "three"
englishDigit 2 = "two"
englishDigit 1 = "one"
englishDigit 0 = "zero"
englishDigit _ = "unknown"

divTuple :: (Eq a, Fractional a) => (a, a) -> a
divTuple (_, 0) = undefined
divTuple (x, y) = x / y

threeZeroList :: [Int] -> Bool
threeZeroList (0:0:0:xs) = True
threeZeroList _ = False

  -- 05 Recursion

stepReverseSign :: (Fractional a, Ord a) => a -> a -> a
stepReverseSign a b = -((abs a) + b)

-- 06 Higher order functions
sumInts :: Int -> Int -> Int
sumInts a b = if a < b then a + sumInts (a+1) b else a

sq :: Int -> Int
sq x = x * x

sumSquares :: Int -> Int -> Int
sumSquares a b = if a < b then sq a + sumSquares (a+1) b else sq a

higherOrderSum :: (Int -> Int) -> Int -> Int -> Int
higherOrderSum intApplication a b = if a < b then intApplication a + higherOrderSum intApplication (a + 1) b else intApplication a

hoSumSquares :: Int -> Int -> Int
hoSumSquares = higherOrderSum sq

hoSumInts :: Int -> Int -> Int
hoSumInts = higherOrderSum (\x -> x)

higherOrderSequenceApplication :: (Int -> Int) ->  (Int -> Int -> Int) -> Int -> Int -> Int
higherOrderSequenceApplication f op a b = if a < b then f a `op` higherOrderSequenceApplication f op (a + 1) b else f b

-- 07 Modules

areaConv :: (Float -> Float) -> Float -> Float
areaConv linearConversion area = linearConversion $ linearConversion area

sqInToSqCm :: Float -> Float
sqInToSqCm = areaConv inchesToCentimetres

sqChainsToSqM :: Float -> Float
sqChainsToSqM = areaConv chainsToMeters

-- 08 Types

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord)
data Digit = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord)
data Card = Card Digit Suit deriving (Eq)

instance Show Card where 
  show (Card digit suit) = "The " ++ show digit ++ " of " ++ show suit

instance Ord Card where
  (<=) (Card digit1 suit1) (Card digit2 suit2) = suit1 <= suit2  && digit1 <= digit2


-- We should be able to provide a function which returns the higher ranked card:
betterCard :: Card -> Card -> Card
betterCard x y = if x > y then x else y

-- Here is a new Typeclass, which represents some kind of playing hand in a game.
-- It returns True for a "winning hand", depending on the rules for the type of class we are playing with
class Hand a where
  play :: [a] -> Bool

-- Implement Hand for Card, where play returns true if the list contains the Ace of Spades
instance Hand Card where
  play c = elem (Card Ace Spades) c

-- Create a new Coin type
data Coin = Heads | Tails deriving (Eq)

-- Implement Hand for Coin, where play returns true if there are ten heads in a row in the list
instance Hand Coin where
  play c 
    | length c < 10 = False
    | otherwise = if  (take 10 c) == (replicate 10 Heads) then True else play (drop 1 c)