module Year2015.Day3.Solution where

import Common (everyFirst, everySecond)
import qualified Data.Map as M

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Int
part1 input = M.size $ snd $ foldl makeStep ((0, 0), M.singleton (0, 0) 1) input

-- (x,y) is previous coordinate
-- steps is a map Coordinate->Count o presents (not used though)
-- produces the new coordinate and 'adds' previous to the map
makeStep :: ((Int, Int), M.Map (Int, Int) Int) -> Char -> ((Int, Int), M.Map (Int, Int) Int)
makeStep ((x, y), steps) c =
  let (x', y') = case c of
        '<' -> (x -1, y)
        '>' -> (x + 1, y)
        '^' -> (x, y + 1)
        'v' -> (x, y -1)
        _ -> (x, y)
   in ((x', y'), M.insertWith (+) (x', y') 1 steps)

part2 :: String -> Int
part2 input =
  let santa = snd $ foldl makeStep ((0, 0), M.singleton (0, 0) 1) $ everyFirst input
      robo = snd $ foldl makeStep ((0, 0), M.singleton (0, 0) 1) $ everySecond input
   in M.size $ M.unionWith (+) santa robo
