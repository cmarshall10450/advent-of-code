module AOC
  ( module AOC,
  )
where

import Control.Exception
import Formatting
import Formatting.Clock
import System.Clock

type Day = Int

data Solution a b c = Solution
  { parse :: String -> a,
    part1 :: a -> b,
    part2 :: a -> c
  }

getInput :: Day -> IO String
getInput d = readFile $ "./input/day" ++ show d ++ ".txt"

benchmark :: IO a -> IO ()
benchmark action = do
  start <- getTime Monotonic
  action
  end <- getTime Monotonic
  fprint (" (" % timeSpecs % ")\n") start end

todo :: a -> String
todo x = "(not implemented)"

solve :: (Show b, Show c) => Int -> Solution a b c -> IO ()
solve day solution = do
  input <- getInput day
  let problem = parse solution input

  benchmark $ do
    putStr "Parsing input..."
    evaluate problem

  putStrLn $ "Part 1: " ++ show (part1 solution problem)
  putStrLn $ "Part 2: " ++ show (part2 solution problem)