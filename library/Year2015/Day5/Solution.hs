module Year2015.Day5.Solution where

import Common

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 input = length $ filter isNice $ lines input

part2 :: String -> Int
part2 = error "not implemented"

isNice :: String -> Bool
isNice xs = not $ all (`contains` xs) ["ab","cd","pq","xy"]

contains :: String -> String -> Bool
contains needle haystack = case findString needle haystack of
  Just _ -> True
  Nothing -> False

--ab, cd, pq, or xy
