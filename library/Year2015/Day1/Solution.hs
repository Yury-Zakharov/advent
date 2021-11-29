module Year2015.Day1.Solution where

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part2 :: String -> Int
part2 xs = fst $ head $ filter (\p -> snd p == -1) $ zipWith foo [1 ..] xs
  where
    foo :: Int -> Char -> (Int, Int)
    foo i _ = (i, part1 $ take i xs)

part1 :: String -> Int
part1 = foldl sortBrackets 0
  where
    sortBrackets cnt c = case c of
      '(' -> cnt + 1
      ')' -> cnt -1
      _ -> cnt
