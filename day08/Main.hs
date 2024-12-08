module Main where

import Data.List (groupBy, nub, sortBy, tails)
import Data.Function (on)
import Grid (Coord, fromList, Grid, enumerate, shape)
import Utils (cart, map2)

type Size    = Coord
type Antenna = Coord


-- Get dimensions of the grid, as well as all antenna positions.
-- Antenna positions are grouped by antenna identifiers; the latter
-- is not returned, as we won't need it anymore.
parseInput :: String -> (Size, [[Antenna]])
parseInput s = (inputShape, antennas)
  where
    inputShape = shape grid
    antennas   = map2 fst
               . groupBy ((==) `on` snd)
               . sortBy (compare `on` snd)
               $ filter (\ x -> snd x /= '.') (enumerate grid)
    grid       = fromList . lines $ s


------------
-- Part 1 --
------------

-- Check whether a coordinate pair is in bounds of the grid.
inBounds :: Size -> Coord -> Bool
inBounds (n, k) (r, c) = r >= 0 && c >= 0 && r < n && c < k


-- Given a pair of antenna positions, calculate the position of the
-- two antinodes (stored in a list for easier processing later).
antinodesPair :: (Antenna, Antenna) -> [Coord]
antinodesPair ((r1, c1), (r2, c2)) = [(r1 - dr, c1 - dc),
                                      (r2 + dr, c2 + dc)]
  where
    (dr, dc) = (r2 - r1, c2 - c1)


-- Given a list of antenna positions, calculate all antinode positions.
antinodesList :: [Antenna] -> [Coord]
antinodesList antennas = concatMap antinodesPair uniquePairs
  where
    uniquePairs = [(a1, a2) | (a1:a2s) <- tails antennas, a2 <- a2s]


------------
-- Part 2 --
------------

-- Given a pair of antenna positions, calculate the position of all
-- the antinodes (stored in a list for easier processing later).
-- We go `maxsteps` in either direction.
antinodesPair' :: Int -> (Antenna, Antenna) -> [Coord]
antinodesPair' maxsteps ((r1, c1), (r2, c2)) = [(r1 + i * dr, c1 + i * dc)
                                             | i <- [(-maxsteps) .. maxsteps]]
  where
    (dr, dc) = (r2 - r1, c2 - c1)


-- Given a list of antenna positions, calculate all antinode positions.
-- We go `maxsteps` in either direction for every pair.
antinodesList' :: Int -> [Antenna] -> [Coord]
antinodesList' maxsteps antennas = concatMap (antinodesPair' maxsteps) uniquePairs
  where
    uniquePairs = [(a1, a2) | (a1:a2s) <- tails antennas, a2 <- a2s]


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let ((n, k), antennas) = parseInput fileContents

    print $ length
          . filter (inBounds (n, k))
          . nub
          . concatMap antinodesList
          $ antennas

    -- For part 2, we set `maxsteps` to the size of the grid, which guarantees
    -- that we will not miss any position on the grid.
    print $ length
          . filter (inBounds (n, k))
          . nub
          . concatMap (antinodesList' n)
          $ antennas
    print $ "Done."

