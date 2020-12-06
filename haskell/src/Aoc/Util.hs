module Aoc.Util where

import Data.List.Split
import Data.List (delete)
import Data.List.Utils (replace)

type Day = Int

data Solution a b c = Solution
  { parse :: String -> a,
    part1 :: a -> b,
    part2 :: a -> c
  }

getInput :: Day -> IO String
getInput d = readFile $ "./input/day" ++ show d ++ ".txt"

count :: (a -> Bool)  -> [a] -> Int
count f = length . filter f

splitByBlankLines :: String -> [String]
splitByBlankLines = splitOn "\n\n"

todo :: a -> String
todo x = "(not implemented)"

solve :: (Show b, Show c) => Int -> Solution a b c -> IO ()
solve day solution = do
  input <- getInput day
  let problem = parse solution input

  putStrLn $ "Part 1: " ++ show (part1 solution problem)
  putStrLn $ "Part 2: " ++ show (part2 solution problem)