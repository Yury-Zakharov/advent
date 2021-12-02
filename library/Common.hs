module Common where

import Control.Applicative
import Data.List

toInt :: String -> Int
toInt = read

splitBy :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitBy delimiter = foldr f [[]]
  where
    f c l@(x : xs)
      | c == delimiter = [] : l
      | otherwise = (c : x) : xs
    f _ _ = []

min3 :: Ord a => a -> a -> a -> a
min3 one two = min (min one two)

max3 :: Ord a => a -> a -> a -> a
max3 one two = max (max one two)

-- | Takes every odd element in a list. [a,b,c,d,e,f] -> [a,c,e]
everyFirst :: [a] -> [a]
everyFirst [] = []
everyFirst (x : xs) = x : everySecond xs

-- | Takes every even element in a list. [a,b,c,d,e,f] -> [b,d,f]
everySecond :: [a] -> [a]
everySecond [] = []
everySecond (_ : xs) = everyFirst xs

transpose' :: [[a]] -> [[a]]
transpose' = getZipList . traverse ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

group' :: Int -> [a] -> [[a]]
group' _ [] = []
group' n l
  | n > 0 = take n l : group' n (drop n l)
  | otherwise = error "Negative or zero n"