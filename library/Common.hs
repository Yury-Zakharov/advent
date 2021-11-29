module Common where

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