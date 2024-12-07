module Main where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (nub, (\\))
import Data.Maybe (fromJust)
import Grid (Coord, enumerate, fromList, Grid, shape)
import Utils (fst3, snd3, thi3)

type Length    = Int
type Obstacles = Set Coord
type GridData  = (Length, Length, Obstacles)
type Direction = Char
type State     = (Coord, Direction)


-- Note: To make use of parallelisation, add the following to the .cabal file:
--    ghc-options:
--        -O3
--        -threaded
--        -rtsopts
--        -with-rtsopts=-N8

-- Parse input into Grid for better extraction of relevant info
parseInput :: String -> Grid Char
parseInput = fromList . lines


-- Extract all relevant information from Grid:
-- Grid dimensions, obstrace positions, starting position and direction.
extractInfo :: Grid Char -> (GridData, State)
extractInfo grid = ((n, k, obs), state0)
  where
    (n, k)  = shape grid
    obs     = Set.fromList
            . map fst
            . filter ((=='#') . snd)
            . enumerate
            $ grid
    state0  = head
            . filter ((`elem` "><^v") . snd)
            . enumerate
            $ grid



-- Take maybe one step, depending on whether we would leave the bounds or not.
-- Return maybe new position and new direction
step :: GridData -> State -> Maybe State
step (n, k, obstacles) ((i, j), d)
    | i < 0 || j < 0 || i == n || j == k         = Nothing
    | d == '^' && (i - 1, j) `Set.notMember` obstacles = Just ((i - 1, j), d)
    | d == '>' && (i, j + 1) `Set.notMember` obstacles = Just ((i, j + 1), d)
    | d == 'v' && (i + 1, j) `Set.notMember` obstacles = Just ((i + 1, j), d)
    | d == '<' && (i, j - 1) `Set.notMember` obstacles = Just ((i, j - 1), d)
    | otherwise                                  = Just ((i, j), d')
  where
    d' = case d of {'^' -> '>'; '>' -> 'v'; 'v' -> '<'; '<' -> '^'}


-- Execute the complete course until we run out of bounds.
course :: GridData -> State -> [Coord]
course gridData ((i, j), d) = case step gridData ((i, j), d) of
    Nothing     -> []
    Just state' -> (i, j) : course gridData state'


------------
-- Part 2 --
------------

-- Cycle detection, using the fact that a state is exactly defined
-- by position and direction.
isCycle :: GridData -> State -> Bool
isCycle gridData = go Set.empty
  where
    go seen state
        | state `Set.member` seen        = True
        | step gridData state == Nothing = False
        | otherwise                      = go seen' state'
      where
        seen'  = state `Set.insert` seen
        state' = fromJust $ step gridData state




main :: IO ()
main = do
    fileContents           <- readFile "input.txt"
    let grid               = parseInput fileContents
    let (gridData, state0) = extractInfo grid

    -- Part 1: Execute the entire course and count the number
    -- of individual cells visited.

    let visited = nub $ course gridData state0
    print $ length visited

    -- Part 2: Try placing an obstacle at every position the guard
    -- has visited in Part 1, but excluding the starting position.
    -- Count number of cyclic courses.

    let candidateObstacles = visited \\ [fst state0]
    let (n, k)             = (fst3 gridData, snd3 gridData)
    let candidategridDatas = map (\ o -> (n, k, Set.insert o (thi3 gridData)))
                           $ candidateObstacles

    print $ length
          . filter id
          . parMap rseq ( \gd -> isCycle gd state0)
          $ candidategridDatas
    print $ "Done."

