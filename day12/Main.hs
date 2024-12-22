module Main where

import Data.List ((\\), nub, group, sort)
import Data.Tuple (swap)
import Grid
import Utils (tok)

-- Sugar
type Cell      = (Coord, Char)
type Region    = [Coord]
type Edgepoint = (Int, Int)
type Edge      = (Edgepoint, Edgepoint)

-- Given the input grid and one cell (coordinate + character),
-- find the contiguous region of cells around the coordinate
-- with the same character.
region :: Grid Char -> (Coord, Char) -> Region
region grid start@(startCoord, startChar) = go [] [startCoord]
                                          $ map fst
                                          . filter ((==startChar) . snd)
                                          . filter ((/=startCoord) . fst)
                                          $ enumerate grid
  where
    go accum queue rest
        | null queue = accum
        | otherwise = go accum' queue' rest'
      where
        accum'              = currentCoord : accum
        queue'              = (tail queue) ++ unvisitedNeighbours
        rest'               = rest \\ unvisitedNeighbours
        currentCoord        = head queue
        unvisitedNeighbours = filter (`elem` rest)
                            . map fst
                            $ vonNeum grid currentCoord


-- Step through the given grid, identify all the contiguous regions
-- one after another.
findRegions :: Grid Char -> [Region]
findRegions grid = go (enumerate grid)
  where
    go :: [Cell] -> [[Coord]]
    go []           = []
    go (cell:cells) = currentRegion
                    : go (filter ((`notElem` currentRegion) . fst) cells)
      where
        currentRegion = region grid cell


-- Calculate the price of a region by multiplying its perimeter and size.
price :: Region -> Int
price r = (perimeter r) * (size r)


-- Trivial sugar to calculate the size of a region
size :: Region -> Int
size = length


-- Helper to calculate the perimeter of a region.
perimeter :: Region -> Int
perimeter = length . regionEdges


-- Get all the edges of a given region.
-- The idea is to enumerate all borders of every
-- cell of the region, then to check which of these
-- borders occur exactly once.
regionEdges :: Region -> [Edge]
regionEdges = map head
            . filter ((==1) . length)
            . group
            . sort
            . concatMap coordEdges


-- Map a given cell coordinate to its four edges.
coordEdges :: Coord -> [Edge]
coordEdges (r, c) = [left, right, upper, lower]
  where
    left  = ((r, c), (r + 1, c))
    right = ((r, c + 1), (r + 1, c + 1))
    upper = ((r, c), (r, c + 1))
    lower = ((r + 1, c), (r + 1, c + 1))


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let grid      = fromList . lines $ fileContents

    -- Part 1: Straightforward finding of regions and adding their prices,
    -- which are calculated by multiplying area and perimeter.
    print $ sum
          . map price
          $ findRegions grid

    print $ "Done."

