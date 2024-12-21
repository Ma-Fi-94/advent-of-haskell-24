module Main where

import Data.List ((\\))
import Grid (Coord, enumerate, fromList)

-- Sugar
type WalkableCoords = [Coord]
type Start          = Coord
type Finish         = Coord
type Path           = [Coord]

-- Helper to calculate the taxicab distance between two points.
cab :: Coord -> Coord -> Int
cab (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)


-- Extract the start coordinate, the finish coordinate and all
-- walkable coordinates from the input.
parseInput :: String -> (WalkableCoords, Start, Finish)
parseInput fileContents = (walkables, start, finish)
  where
    walkables = map fst . filter ((`elem` ".SE") . snd) $ assocs
    start     = fst . head . filter ((=='S') . snd) $ assocs
    finish    = fst . head . filter ((=='E') . snd) $ assocs
    assocs    = enumerate . fromList . lines $ fileContents


-- Find the regular path through the maze. Careful inspection of
-- the input shows that the path is effectively linear, so we do
-- not need to use any pathfinding algorithms, but instead just
-- have to take the only possible next step (from the von-Neumann
-- neighbourhood of the current position) every time until we reach
-- the finish coordinate.
-- For my input, this runs in ~1.4s (when compiled -O3), which I
-- consider acceptable. An obvious improvement would be using a
-- Set of Coords, but this seems overkill right now.
findPath :: (WalkableCoords, Start, Finish) -> Path
findPath (walkables, start, finish) = go [] start (walkables \\ [start])
  where
    go path current@(row, col) unvisiteds
        | current == finish = path ++ [finish]
        | otherwise         = go (path ++ [current]) current' unvisiteds'
      where
        current'    = head
                    . filter (`elem` [(row, col + 1),
                                      (row, col - 1),
                                      (row + 1, col),
                                      (row - 1, col)])
                    $ unvisiteds
        unvisiteds' = unvisiteds \\ [current'] 


-- Simple helper to annoate every step of the found path with its
-- distance to the finish.
annotatePath :: Path -> [(Coord, Int)]
annotatePath = reverse . (`zip` [0..]) . reverse


main :: IO ()
main = do
    fileContents  <- readFile "input.txt"
    let input     =  parseInput fileContents
    let track     =  annotatePath . findPath $ input

    -- Part 1: We just enumerate all pairs of fields on the track,
    -- which have a taxicab distance of at most 2. For all of these,
    -- we calculate the gain, and count how many have a gain of
    -- at least 100.
    print $ length
          . filter (>=100)
          $ [gain | (coord1, dist1) <- track,
                    (coord2, dist2) <- track,
                    dist1 < dist2,
                    cab coord1 coord2 <= 2,
                    let gain = dist2 - dist1 - cab coord1 coord2]


    -- Part 2: Like Part 1, but the taxicab distance may now be at
    -- most 20, instead of 2.
    print $ length
          . filter (>=100)
          $ [gain | (coord1, dist1) <- track,
                    (coord2, dist2) <- track,
                    dist1 < dist2,
                    cab coord1 coord2 <= 20,
                    let gain = dist2 - dist1 - cab coord1 coord2]

    print $ "Done."

