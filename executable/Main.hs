import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Year2015.Day1.Solution as Y2015D1
import qualified Year2015.Day2.Solution as Y2015D2
import qualified Year2015.Day3.Solution as Y2015D3
import qualified Year2015.Day4.Solution as Y2015D4
import qualified Year2021.Day1.Solution as Y2021D1
import qualified Year2015.Day5.Solution as Y2015D5
import qualified Year2021.Day2.Solution as Y2021D2

main :: IO ()
main = getArgs >>= parse >>= resolve

resolve :: (String, String) -> IO ()
resolve (year, day) = do
  content <- loadInput year day
  case year of
    "2015" -> case day of
      "1" -> Y2015D1.solve content
      "2" -> Y2015D2.solve content
      "3" -> Y2015D3.solve content
      "4" -> Y2015D4.solve content
      "5" -> Y2015D5.solve content
      _ -> print "dunno yet"
    "2021" -> case day of
      "1" -> Y2021D1.solve content
      "2" -> Y2021D2.solve content
      _ -> print "dunno yet"
    _ -> print "NIY"

parse :: [String] -> IO (String, String)
parse ["-h"] = usage >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse (year : day : _) = return (year, day)
parse _ = usage >> exitSuccess

usage :: IO ()
usage = putStrLn "Usage: advent [-vh] year day"

version :: IO ()
version = putStrLn "advent ov code 0.1"

-- need to check for file eistence
loadInput :: String -> String -> IO String
loadInput year day = readFile $ "library/Year" ++ year ++ "/Day" ++ day ++ "/input.txt"
