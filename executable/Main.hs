import System.Environment (getArgs)
import System.Exit (exitSuccess)
import qualified Year2015.Day1.Solution as Y2015D1

main :: IO ()
main = getArgs >>= parse >>= resolve

resolve :: (String, String) -> IO ()
resolve (year, day) = case year of
  "2015" -> case day of
    "1" -> do
      content <- loadInput year day
      Y2015D1.solve content
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

loadInput :: String -> String -> IO String
loadInput year day = readFile $ "library/Year" ++ year ++ "/Day" ++ day ++ "/input.txt"
