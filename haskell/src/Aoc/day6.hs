module Aoc.Day6 where

import Aoc.Util
import Data.List.Split
import Data.List.Utils (replace)
import qualified Data.Set as S
import Data.List (foldl1', nub)

run :: IO()
run = do
  solve 6 Solution {
    parse = splitByBlankLines,
    part1 = sum . map (length . nub . replace "\n" ""),
    part2 = sum . map (length . foldl1' S.intersection . map S.fromList . splitOn "\n")
}