module Main where

import Data.List (reverse, transpose)
import Data.Universe.Helpers (diagonals)

-- Construct all length-i sublists of a given list.
sublists :: Int -> [a] -> [[a]]
sublists i xs
    | i > length xs = []
    | otherwise     = (take i xs) : sublists i (tail xs)


-- Count how often a list appears as a sublist within another.
count :: Eq a => [a] -> [a] -> Int
count ndl = length . filter (==ndl) . sublists (length ndl)


-- Part 1: Count how often "XMAS" appears in the entire grid.
-- < 2604
part1 :: [[Char]] -> Int
part1 grid = sum $ map (count "XMAS") searchspace
  where
    searchspace = grid
                ++ map reverse grid
                ++ transpose grid
                ++ map reverse (transpose grid)
                ++ diagonals grid
                ++ map reverse (diagonals grid)
                ++ diagonals ((map reverse) grid)
                ++ map reverse (diagonals ((map reverse) grid))

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let grid    = lines $ fileContents

    print $ part1 grid

    print $ "Done."

