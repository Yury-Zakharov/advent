-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.

-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.

import Common (everyFirst, everySecond)
import qualified Test.Tasty
import Test.Tasty.Hspec
import qualified Year2015.Day3.Solution as Y2015D3
-- import qualified Year2015.Day4.Solution as Y2015D4

import qualified Year2015.Day5.Solution as Y2015D5
import qualified Year2021.Day1.Solution as Y2021D1

main :: IO ()
main = do
  test <- testSpec "advent" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  it "first things first" $ do
    everyFirst ['a' .. 'z'] `shouldBe` ['a', 'c' .. 'y']
    everySecond ['a' .. 'z'] `shouldBe` ['b', 'd' .. 'z']
  it "Santa should visit homes" $ do
    Y2015D3.part1 ">" `shouldBe` 2
    Y2015D3.part1 "^>v<" `shouldBe` 4
    Y2015D3.part1 "^v^v^v^v^v" `shouldBe` 2
  it "Robo-Santa should visit houses too" $ do
    Y2015D3.part2 "^v" `shouldBe` 3
    Y2015D3.part2 "^>v<" `shouldBe` 3
    Y2015D3.part2 "^v^v^v^v^v" `shouldBe` 11
  -- it "Santa is mining" $ do
  --   Y2015D4.part1 "abcdef" `shouldBe` 609043
  --   Y2015D4.part1 "pqrstuv" `shouldBe` 1048970
  it "we all live in the.." $ do
    Y2021D1.part1 measurements `shouldBe` 7
    Y2021D1.part2 measurements `shouldBe` 5
  it "String primitives" $ do
    Y2015D5.hasDups "ugknbfddgicrmopn" `shouldBe` True
    Y2015D5.hasDups "ugknbfdadgicrmopn" `shouldBe` False
    Y2015D5.threeWowels "ugknbfddgicrmopn" `shouldBe` True
    Y2015D5.threeWowels "acdegjhijklmno" `shouldBe` True
  it "Nice strings" $ do
    Y2015D5.isNice "ugknbfddgicrmopn" `shouldBe` True
    Y2015D5.isNice "aaa" `shouldBe` True
    Y2015D5.isNice "jchzalrnumimnmhp" `shouldBe` False
    Y2015D5.isNice "haegwjzuvuyypxyu" `shouldBe` False
    Y2015D5.isNice "dvszwmarrgswjxmb" `shouldBe` False
  it "Nice strings too" $ do
    Y2015D5.part2 "qjhvhtzxzqqjkmpb" `shouldBe` 1
    Y2015D5.part2 "xxyxx" `shouldBe` 1
    Y2015D5.part2 "uurcxstgmygtbstg" `shouldBe` 0
    Y2015D5.part2 "ieodomkazucvgmuy" `shouldBe` 0
    Y2015D5.hasDoublePair "qjhvhtzxzqqjkmpb" `shouldBe` True
    Y2015D5.hasDoublePair "aaaa" `shouldBe` True

measurements :: String
measurements = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263\n"
