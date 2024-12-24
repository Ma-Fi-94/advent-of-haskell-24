module Main where

import Data.List ((\\), nub, group, sort)
import Data.Tuple (swap)
import Grid
import Utils (tok)

import Debug.Trace (trace)

-- Sugar
type Cell      = (Coord, Char)
type Region    = [Coord]
type Edge      = (Coord, Char)

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
price region = (length (regionEdges region)) * (length region)



-- Get all the edges of a given region.
regionEdges :: Region -> [Edge]
regionEdges region = filter (not . isInside)
                   $ [(coord, position) | coord <- region,
                                          position <- "NSWE"]
  where
    isInside ((r, c), 'N') = (r - 1, c) `elem` region
    isInside ((r, c), 'S') = (r + 1, c) `elem` region
    isInside ((r, c), 'W') = (r, c - 1) `elem` region
    isInside ((r, c), 'E') = (r, c + 1) `elem` region


------------
-- Part 2 --
------------

-- Assemble the border of a region. Caution: A border may have more than one
-- border, namely if it has holes inside. Hence, we return a list of borders.
assembleBorder :: [Edge] -> [[Edge]]
assembleBorder (edge:edges) = go [] edge edges
  where
    go accum current rest
        | null rest = [accum ++ [current]]
        | otherwise = case findNext current of
                          Just next -> go (accum ++ [current]) next (rest \\ [next])
                          Nothing   -> (accum ++ [current]) : assembleBorder rest
      where
        findNext ((r, c), d)
            -- Same cell, go along its border counterclockwise 
            | d == 'N' && ((r, c), 'W') `elem` rest = Just ((r, c), 'W')
            | d == 'W' && ((r, c), 'S') `elem` rest = Just ((r, c), 'S')
            | d == 'S' && ((r, c), 'E') `elem` rest = Just ((r, c), 'E')
            | d == 'E' && ((r, c), 'N') `elem` rest = Just ((r, c), 'N')

            -- Continue edge in a straight line to adjacent cell
            | d == 'N' && ((r, c - 1), 'N') `elem` rest = Just ((r, c - 1), 'N')
            | d == 'W' && ((r + 1, c), 'W') `elem` rest = Just ((r + 1, c), 'W')
            | d == 'S' && ((r, c + 1), 'S') `elem` rest = Just ((r, c + 1), 'S')
            | d == 'E' && ((r - 1, c), 'E') `elem` rest = Just ((r - 1, c), 'E')

            -- Corners in counterclockwise direction
            | d == 'N' && ((r - 1, c - 1), 'E') `elem` rest = Just ((r - 1, c - 1), 'E')
            | d == 'S' && ((r + 1, c + 1), 'W') `elem` rest = Just ((r + 1, c + 1), 'W')
            | d == 'W' && ((r + 1, c - 1), 'N') `elem` rest = Just ((r + 1, c - 1), 'N')
            | d == 'E' && ((r - 1, c + 1), 'S') `elem` rest = Just ((r - 1, c + 1), 'S')

            -- Some rest we need to assemble in a next step (holes?)
            | otherwise = Nothing



-- Find the number of linear segments of a region.
-- If the first and the large edge of the border have the same
-- orientation, we need to subtract 1.
nbSegments :: Region -> Int
nbSegments region = sum
                  $ map (\border -> if   cornerAtStart border
                                    then nbGroups border
                                    else (nbGroups border) - 1)
                    borders


  where
    borders              = assembleBorder . regionEdges $ region
    nbGroups border      = length . group . map snd $ border
    cornerAtStart border = (snd (head border)) /= (snd (last border))


-- Calculate the price of a region by multiplying its number of linear
-- border segments and it size.
price' :: Region -> Int
price' region = (nbSegments region) * (length region)


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let grid      = fromList . lines $ fileContents

    -- Part 1: Straightforward finding of regions and adding their prices,
    -- which are calculated by multiplying area and perimeter.
    print $ sum
          . map price
          $ findRegions grid

    -- Part 2
    print $ sum
          . map price'
           $ findRegions grid

    print $ "Done."

