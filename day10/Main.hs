module Main where

import Data.List (nub)
import Grid (Coord, Grid, enumerate, fromList, vonNeum)
import Utils (map2, readDigits)

type Cell = (Coord, Int)


-- Sugar

getPosition :: Cell -> Coord
getPosition = fst

getElevation :: Cell -> Int
getElevation = snd


-- Given a Grid and a Cell, find all suitable next cells on a trail.
-- Those are all von-Neumann neighbours that have an elevation one
-- unit above the current cell.
nextSteps :: Grid Int -> Cell -> [Cell]
nextSteps grid (position, elevation) = filter isOneAbove neighbours
  where
    neighbours = vonNeum grid position
    isOneAbove = (==(elevation + 1)) . getElevation


-- Get the number of all possible terminal cells with a '9',
-- when starting at a given cell.
nbTerminals :: Grid Int -> Cell -> Int
nbTerminals grid cell = length . nub . map fst $ go [cell]
  where
    go [] = []
    go cs = terminalCells ++ go (concatMap (nextSteps grid) nonTerminalCells)
      where
        terminalCells    = filter ((==9) . getElevation) cs
        nonTerminalCells = filter ((/=9) . getElevation) cs

main :: IO ()
main = do
    fileContents   <- readFile "input.txt"
    let grid       = fromList . map readDigits . lines $ fileContents
    let startCells = filter ((==0) . getElevation) $ enumerate grid

    print $ sum $ map (nbTerminals grid) startCells

    print $ "Done."

