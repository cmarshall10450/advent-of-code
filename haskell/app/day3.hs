module Main where

import AOC

data Space = Ground | Tree deriving (Show)

takeNth m = map snd . filter (\(x, y) -> (x `mod` m) == 0) . zip [0 ..]

isTree :: Space -> Bool
isTree s = case s of
  Tree -> True
  _ -> False

parseSpace :: Char -> Space
parseSpace c = case c of
  '#' -> Tree
  '.' -> Ground
  _ -> Ground

makeGrid :: [String] -> [[Space]]
makeGrid l = map parseSpace . cycle <$> l

solvePart1 :: Int -> Int -> [String] -> Int
solvePart1 x y l = length $ filter isTree slope
  where
    grid = makeGrid l
    slope = [snd i !! (fst i * x) | i <- zip [0 ..] $ takeNth y grid]

solvePart2 :: [String] -> [(Int, Int)] -> Int
solvePart2 = (product .) . map . flip (uncurry solvePart1)

main :: IO ()
main = do
  let allSlopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  solve
    3
    Solution
      { parse = lines,
        part1 = solvePart1 3 1,
        part2 = flip solvePart2 allSlopes
      }