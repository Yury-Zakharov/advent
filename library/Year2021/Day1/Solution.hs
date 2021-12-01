module Year2021.Day1.Solution where

import Common (toInt, windows)

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 input = length $ filter increasing $ zip measurments (drop 1 measurments)
  where
    measurments = map toInt $ lines input
    increasing :: (Int, Int) -> Bool
    increasing (c, n) = n > c

part2 :: String -> Int
part2 input = length $ filter increasing $ zip measurments (drop 1 measurments)
  where
    measurments = map sum $ windows 3 $ map toInt $ lines input
    increasing :: (Int, Int) -> Bool
    increasing (c, n) = n > c
