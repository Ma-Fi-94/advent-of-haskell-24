module Main where

import Data.Char (isDigit)
import Data.List (group, sort)
import Data.Maybe (fromJust, isJust)
import Utils (dropWhile1, readInt, tok)

type Position = (Int, Int)
type Velocity = (Int, Int)
type Robot    = (Position, Velocity)

-- We need to hard-code the size of the board here
sizex :: Int
sizex = 101

sizey :: Int
sizey = 103


-- Does exactly what it says on the tin
parseLine :: String -> Robot
parseLine line = ((px, py), (vx, vy))
  where
    px       = readInt . takeWhile isDigi $ drop 2 position
    py       = readInt . takeWhile isDigi $ dropWhile1 (/=',') position
    vx       = readInt . takeWhile isDigi $ drop 2 velocity
    vy       = readInt . takeWhile isDigi $ dropWhile1 (/=',') velocity
    position = (tok " " line) !! 0
    velocity = (tok " " line) !! 1
    isDigi   = \ c -> isDigit c || c == '-' 


-- Propagate a robot a given number of steps.
step :: Int -> Robot -> Position
step n ((x0, y0), (vx, vy)) = (x, y)
  where
    x = (x0 + n * vx) `mod` sizex
    y = (y0 + n * vy) `mod` sizey


-- Given a robot position, determine in which quadrant it is
-- Quadrants are counted starting in the upper right,
-- progressing counter-clockwise.
quadrant :: Position -> Maybe Int
quadrant (x, y)
    | x > (sizex - 1) `div` 2 && y < (sizey - 1) `div` 2 = Just 1
    | x < (sizex - 1) `div` 2 && y < (sizey - 1) `div` 2 = Just 2
    | x < (sizex - 1) `div` 2 && y > (sizey - 1) `div` 2 = Just 3
    | x > (sizex - 1) `div` 2 && y > (sizey - 1) `div` 2 = Just 4
    | otherwise                                          = Nothing


-- Calculate the score, based on a list of robot positions
score :: [Position] -> Int
score = product
      . map length
      . group
      . sort
      . map fromJust
      . filter isJust
      . map quadrant


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let robots   = map parseLine . lines $ fileContents

    -- Part 1
    print $ score
          . map (step 100)
          $ robots
    print $ "Done."

