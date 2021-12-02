module Year2015.Day5.Solution where

import Common (windows)

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 input = length $ filter isNice $ lines input

part2 :: String -> Int
part2 input = length $ filter isNice' $ lines input

isNice' :: String -> Bool
isNice' xs = hasMagicPair xs && hasDoublePair xs

hasDoublePair :: String -> Bool
hasDoublePair xs = (or $ checkPairs $ windows 2 xs) || (any (\[a, b, c, d] -> a == b && b == c && c == d) $ windows 4 xs)

-- very ad hoc unction. checks whether the pair o chars is present in a list (o pairs o chars) and not produced by windowing
-- over an overlapping chars (three identical chars)
checkPairs :: Eq a => [[a]] -> [Bool]
checkPairs [] = []
--checkPairs [x] = []
checkPairs (x : xs) =
  (x `elem` xs && x /= head xs) : checkPairs xs

hasMagicPair :: String -> Bool
hasMagicPair xs = any (\(x : _ : y : _) -> x == y) $ windows 3 xs

isNice :: String -> Bool
isNice xs = not (hasBadThings xs) && hasDups xs && threeWowels xs

hasDups :: String -> Bool
hasDups xs = any isDup $ windows 2 xs

isDup :: [Char] -> Bool
isDup [x, y] = x == y
isDup _ = False

threeWowels :: String -> Bool
threeWowels = hasWowels 3

hasWowels :: Int -> String -> Bool
hasWowels n xs = length (filter isWowel xs) >= n

isWowel :: Char -> Bool
isWowel c = c `elem` wowels

wowels :: [Char]
wowels = "aeiou"

hasBadThings :: String -> Bool
hasBadThings xs = any (`elem` badThings) $ windows 2 xs

badThings :: [[Char]]
badThings = ["ab", "cd", "pq", "xy"]
