import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Year2015.Day1.Solution as Y2015D1
import qualified Year2015.Day2.Solution as Y2015D2

main :: IO ()
main = getArgs >>= parse >>= resolve

resolve :: (String, String) -> IO ()
resolve (year, day) = do
  content <- loadInput year day
  case year of
    "2015" -> case day of
      "1" -> do
        Y2015D1.solve content
      "2" -> do
        Y2015D2.solve content
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
