module Main where

import Data.List (reverse, transpose)
import Data.Universe.Helpers (diagonals)
import Utils (countOcc)

type Grid a = [[a]]

------------
-- Part 1 --
------------

-- Count how often "XMAS" appears in the entire grid.
part1 :: Grid Char -> Int
part1 grid = sum $ map (countOcc "XMAS") searchspace
  where
    searchspace = grid
                ++ map reverse grid
                ++ transpose grid
                ++ map reverse (transpose grid)
                ++ diagonals grid
                ++ map reverse (diagonals grid)
                ++ diagonals ((map reverse) grid)
                ++ map reverse (diagonals ((map reverse) grid))

------------
-- Part 2 --
------------

-- Given a grid, extract the element at (i, j). Partial function!
getCell :: Grid a -> (Int, Int) -> a
getCell g (i, j) = (g !! i) !! j


-- Given a grid, extract all possible X-shaped combinations of cells 
getAllX :: Grid Char -> [[Char]]
getAllX g = go 0 0
  where
    (n, k) = (length g, length (g !! 0))
    go i j
        | i > n - 3 = []
        | j > k - 3 = go (i + 1) 0
        | otherwise = map (getCell g) [(i, j), (i, j + 2),
                                       (i + 1, j + 1),
                                       (i + 2, j), (i + 2, j + 2)]
                    : go i (j + 1)

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let grid    = lines $ fileContents

    print $ part1 grid

    print $ length
          . filter (`elem` ["MMASS", "SMASM", "SSAMM", "MSAMS"])
          $ getAllX grid

    print $ "Done."

