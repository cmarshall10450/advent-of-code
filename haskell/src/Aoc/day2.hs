module Aoc.Day2 where

import Aoc.Util
import Data.List.Split

type Secret = String

data Policy = Policy
  { pMin :: Int,
    pMax :: Int,
    letter :: Char
  }
  deriving (Show)

data Password = Password
  { policy :: Policy,
    secret :: Secret
  }
  deriving (Show)

parsePassword :: String -> Password
parsePassword pwd = Password {policy = policy, secret = secret}
  where
    parts = splitOn ": " pwd
    policy = parsePolicy $ head parts
    secret = last parts

parsePolicy :: String -> Policy
parsePolicy policy = Policy {pMin = pMin, pMax = pMax, letter = letter}
  where
    parts = splitOn " " policy
    letter = head $ last parts
    minmax = splitOn "-" $ head parts
    pMin = read (head minmax)
    pMax = read (last minmax)

meetsOldPolicy :: Password -> Bool
meetsOldPolicy pwd = charCount >= countMin && charCount <= countMax
  where
    countMin = pMin $ policy pwd
    countMax = pMax $ policy pwd
    charCount = length $ filter (== (letter $ policy pwd)) $ secret pwd

meetsNewPolicy :: Password -> Bool
meetsNewPolicy pwd = (pSecret !! idx1 == secretLetter) /= (pSecret !! idx2 == secretLetter)
  where
    pSecret = secret pwd
    idx1 = (pMin $ policy pwd) - 1
    idx2 = (pMax $ policy pwd) - 1
    secretLetter = letter $ policy pwd

run :: IO ()
run = do
  solve
    2
    Solution
      { parse = lines,
        part1 = length . filter meetsOldPolicy . map parsePassword,
        part2 = length . filter meetsNewPolicy . map parsePassword
      }
