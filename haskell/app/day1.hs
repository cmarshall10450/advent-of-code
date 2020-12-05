module Main where

import AOC

main :: IO ()
main = do
  solve
    1
    Solution
      { parse = map (read :: String -> Int) . lines,
        part1 = \list -> head [x * y | x <- init list, y <- tail list, x + y == 2020],
        part2 = \list -> head [x * y * z | x <- list, y <- list, z <- list, x + y + z == 2020]
      }
