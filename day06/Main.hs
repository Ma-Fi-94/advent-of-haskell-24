module Main where

import Data.List (nub)
import Grid (Coord, enumerate, fromList, Grid, shape)

type Dir = Char

-- Parse input into Grid for better extraction of relevant info
parseInput :: String -> Grid Char
parseInput = fromList . lines


-- Extract all relevant information from Grid:
-- Grid dimensions, obstrace positions, starting position and direction.
extractInfo :: Grid Char -> (Int, Int, [Coord], Coord, Dir)
extractInfo grid = (n, k, obs, pos, dir)
  where
    (n, k)     = shape grid
    (pos, dir) = head
               . filter ((`elem` "><^v") . snd)
               . enumerate
               $ grid
    obs        = map fst
               . filter ((=='#') . snd)
               . enumerate
               $ grid

-- Step forward until OOB and keep track of all visited positions
part1 :: Int -> Int -> [Coord] -> Coord -> Dir -> [Coord]
part1 n k obstacles = go
  where
    go (i, j) d
        | i < 0 || j < 0 || i == n || j == k = []
        | d == '^' && (i - 1, j) `notElem` obstacles = (i, j) : go (i - 1, j) d
        | d == '>' && (i, j + 1) `notElem` obstacles = (i, j) : go (i, j + 1) d
        | d == 'v' && (i + 1, j) `notElem` obstacles = (i, j) : go (i + 1, j) d
        | d == '<' && (i, j - 1) `notElem` obstacles = (i, j) : go (i, j - 1) d
        | otherwise                                  = go (i, j) d'
      where
        d' = case d of {'^' -> '>'; '>' -> 'v'; 'v' -> '<'; '<' -> '^'}


main :: IO ()
main = do
    fileContents              <- readFile "input.txt"
    let grid                  = parseInput fileContents
    let (n, k, obs, pos, dir) = extractInfo grid

    print $ length
          . nub
          $ part1 n k obs pos dir






    print $ "Done."

