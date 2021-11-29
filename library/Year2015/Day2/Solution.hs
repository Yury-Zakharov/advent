module Year2015.Day2.Solution where

import Common

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part2 :: String -> Int
part2 input = foldl sumRibbons 0 $ map parse $ lines input

part1 :: String -> Int
part1 input = foldl sumOrders 0 $ map parse $ lines input

sumOrders :: Int -> (Int, Int, Int) -> Int
sumOrders acc (l, w, h) = acc + (2 * l * w + 2 * w * h + 2 * h * l) + min3 (h * l) (l * w) (w * h)

sumRibbons :: Int -> (Int, Int, Int) -> Int
sumRibbons acc (l, w, h) = acc + wrap + bow
  where
    wrap = 2 * l + 2 * w + 2 * h - 2 * max3 l w h
    bow = l * w * h

-- | takes a string like 11x25x22 and retrns a tuple o 3 ints (l,w,h)
parse :: String -> (Int, Int, Int)
parse xs = toOrder $ map toInt $ splitBy 'x' xs
  where
    toOrder (l : w : h : _) = (l, w, h)
    toOrder _ = (0, 0, 0)
