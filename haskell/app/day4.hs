module Main where

import AOC
import Data.Char
import Data.List.Split
import Data.List.Utils (replace)
import qualified Data.Map as M
import Data.Maybe

type Document = M.Map String String

between :: Int -> Int -> Int -> Bool
between x y v = v >= x && v <= y

isValidDocument :: Document -> Bool
isValidDocument = flip all requiredAttrs . flip M.member
  where
    requiredAttrs = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValidDocument' :: Document -> Bool
isValidDocument' d = isValidDocument d && all (\v -> snd v $ fromJust $ M.lookup (fst v) d) validations
  where
    validations =
      [ ("byr", between 1920 2002 . (read :: String -> Int)),
        ("iyr", between 2010 2020 . (read :: String -> Int)),
        ("eyr", between 2020 2030 . (read :: String -> Int)),
        ( "hgt",
          \s ->
            case dropWhile isDigit s of
              "cm" -> between 150 193 $ (read :: String -> Int) s
              "in" -> between 59 76 $ (read :: String -> Int) s
              _ -> False
        ),
        ( "hcl",
          \s ->
            length s == 7 && head s == '#'
              && all (\x -> isDigit x || x `elem` ['a' .. 'f']) (tail s)
        ),
        ("ecl", flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
        ("pid", \s -> length s == 9 && all isDigit s)
      ]

solvePart :: (Document -> Bool) -> [String] -> Int
solvePart f = length . filter (== True) . map (f . M.fromList . map ((\x -> (head x, last x)) . splitOn ":") . splitOn " " . replace "\n" " ")

main :: IO ()
main = do
  solve
    4
    Solution
      { parse = splitOn "\n\n",
        part1 = solvePart isValidDocument,
        part2 = solvePart isValidDocument'
      }