module Aoc.Day5 where

import Aoc.Util
import Control.Monad (liftM2)
import Data.List

generateWindows :: Int -> [a] -> [[a]]
generateWindows windowSize list@(_ : t)
  | length list == windowSize = [window]
  | otherwise = window : rest
  where
    window = take windowSize list
    rest = generateWindows windowSize t

binaryPart :: Char -> Char -> String -> [a] -> a
binaryPart _ _ [] [x] = x
binaryPart a b (x : xs) l =
  let parts = splitAt (length l `div` 2) l
      newList = case x of
        y
          | y == a -> fst parts
          | y == b -> snd parts
          | otherwise -> []
   in binaryPart a b xs newList

seatIds :: [String] -> [Int]
seatIds = map (\x -> binaryPart 'F' 'B' (take 7 x) [0 .. 127] * 8 + binaryPart 'L' 'R' (drop 7 x) [0 .. 7])

run :: IO ()
run = do
  solve
    5
    Solution
      { parse = lines,
        part1 = maximum . seatIds,
        part2 = head . map ((1 +) . head) . filter ((2 ==) . liftM2 (-) last head) . generateWindows 2 . sort . seatIds
      }