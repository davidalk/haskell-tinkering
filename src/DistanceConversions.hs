module DistanceConversions
( yardsToFeet
, feetToInches
, inchesToCentimetres
, chainsToMeters
) where

-- Define yards to feet
yardsToFeet ::  Float -> Float
yardsToFeet y = y * 3

-- Define feet to inches
feetToInches :: Float -> Float
feetToInches f = f * 12

-- Define inches to centimetres
inchesToCentimetres :: Float -> Float
inchesToCentimetres i = i * 2.54

chainsToMeters :: Float -> Float
chainsToMeters i = i * 20.1168