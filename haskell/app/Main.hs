module Main where

import GHC.Conc (getNumProcessors, setNumCapabilities)
import System.Environment (getArgs)
import qualified Aoc.Day1 as Day1
import qualified Aoc.Day2 as Day2
import qualified Aoc.Day3 as Day3
import qualified Aoc.Day4 as Day4
import qualified Aoc.Day5 as Day5
import qualified Aoc.Day6 as Day6

usage :: IO ()
usage = putStrLn "Usage: aoc <year> <day>"

main :: IO ()
main = do
    setNumCapabilities =<< getNumProcessors
    args <- getArgs
    case args of
        []        -> usage
        [_]       -> usage
        (year:xs) -> case year of
            "2020" -> mapM_ run2020 xs
            _      -> putStrLn "Invalid year"

run2020 :: String -> IO ()
run2020 "1" = Day1.run
run2020 "2" = Day2.run
run2020 "3" = Day3.run
run2020 "4" = Day4.run
run2020 "5" = Day5.run
run2020 "6" = Day6.run