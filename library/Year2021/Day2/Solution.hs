module Year2021.Day2.Solution where

import Common (toInt)

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

data Direction = Up | Down | Forward

part1 :: String -> Int
part1 input = position * depth
  where
    (position, depth) = foldl move (0, 0) $ map toCommand $ lines input

part2 :: String -> Int
part2 input = position * depth
  where
    (position, depth, _) = foldl move' (0, 0, 0) $ map toCommand $ lines input

move' :: (Int, Int, Int) -> (Direction, Int) -> (Int, Int, Int)
move' (position, depth, aim) (dir, n) = case dir of
  Up -> (position, depth, aim - n)
  Down -> (position, depth, aim + n)
  Forward -> (position + n, depth + (aim * n), aim)

move :: (Int, Int) -> (Direction, Int) -> (Int, Int)
move (position, depth) (dir, n) = case dir of
  Up -> (position, depth - n)
  Down -> (position, depth + n)
  Forward -> (position + n, depth)

toCommand :: String -> (Direction, Int)
toCommand command =
  let [d, l] = words command
   in case d of
        "forward" -> (Forward, toInt l)
        "up" -> (Up, toInt l)
        "down" -> (Down, toInt l)
        _ -> (Forward, 0)
