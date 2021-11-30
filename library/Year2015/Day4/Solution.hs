module Year2015.Day4.Solution where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Digest.Pure.MD5 (md5)

solve :: String -> IO ()
solve input = do
  print $ part1 input
  print $ part2 input

part1 :: String -> Integer
part1 secret = head $ filter (fiveZeroes secret) [1 :: Integer ..]

part2 :: String -> Integer
part2 secret = head $ filter (sixZeroes secret) [1 :: Integer ..]

fiveZeroes :: String -> Integer -> Bool
fiveZeroes = nZeroes 5

sixZeroes :: String -> Integer -> Bool
sixZeroes = nZeroes 6

nZeroes :: Int -> String -> Integer -> Bool
nZeroes n secret nounce = all (== '0') $ take n $ show $ md5 $ C.pack (secret ++ show nounce)
