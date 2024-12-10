module Main where

import Data.List (nub)
import Grid (Coord, Grid, enumerate, fromList, vonNeum)
import Utils (map2, readDigits)

type Cell  = (Coord, Int)
type Trail = [Cell]


-- Sugar

getPosition :: Cell -> Coord
getPosition = fst

getElevation :: Cell -> Int
getElevation = snd

isComplete :: Trail -> Bool
isComplete = (==9) . getElevation . last


-- Given a Grid and a Cell, find all suitable next cells on a trail.
-- Those are all von-Neumann neighbours that have an elevation one
-- unit above the current cell.
nextSteps :: Grid Int -> Cell -> [Cell]
nextSteps grid (position, elevation) = filter isOneAbove neighbours
  where
    neighbours = vonNeum grid position
    isOneAbove = (==(elevation + 1)) . getElevation


-- Get the number of all possible terminal cells with a '9',
-- reachable from a given starting cell.
nbTerminals :: Grid Int -> Cell -> Int
nbTerminals grid cell = length . nub . map fst $ go [cell]
  where
    go [] = []
    go cs = terminalCells ++ go nextCells
      where
        terminalCells    = filter ((==9) . getElevation) cs
        nonTerminalCells = filter ((/=9) . getElevation) cs
        nextCells        = concatMap (nextSteps grid) nonTerminalCells


------------
-- Part 2 --
------------

-- Given a Grid and a Trail, find all possible trails that emerge
-- from taking one single step onto a suitable (see above) next cell.
trailContinuations :: Grid Int -> Trail -> [Trail]
trailContinuations grid trail = [trail ++ [next] |
                                 next <- nextSteps grid (last trail)]



-- Given a starting position, find all possible trails ending
-- in a nine.
nbAllTrails :: Grid Int -> Cell -> Int
nbAllTrails grid cell = length . nub $ go [[cell]]
  where
    go [] = []
    go ts = completeTrails ++ go growingTrails
      where
        completeTrails   = filter isComplete ts
        incompleteTrails = filter (not . isComplete) ts
        growingTrails    = concatMap (trailContinuations grid) incompleteTrails



main :: IO ()
main = do
    fileContents   <- readFile "input.txt"
    let grid       = fromList . map readDigits . lines $ fileContents
    let startCells = filter ((==0) . getElevation) $ enumerate grid

    print $ sum $ map (nbTerminals grid) startCells

    print $ sum $ map (nbAllTrails grid) startCells

    print $ "Done."

