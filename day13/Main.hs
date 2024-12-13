module Main where

import Data.Char (isDigit)
import Utils (dropWhile1, readInt, tok)

type Machine = ((Int, Int, Int, Int), (Int, Int))


parseMachine :: [String] -> Machine
parseMachine (l1:l2:l3:[]) = ((a11, a12, a21, a22), (p1, p2))
  where
    a11 = readInt . takeWhile isDigit . dropWhile1 (/='+') $ l1
    a21 = readInt . takeWhile isDigit . dropWhile1 (/='+') . dropWhile1 (/='+') $ l1
    a12 = readInt . takeWhile isDigit . dropWhile1 (/='+') $ l2
    a22 = readInt . takeWhile isDigit . dropWhile1 (/='+') . dropWhile1 (/='+') $ l2
    p1  = readInt . takeWhile isDigit . dropWhile1 (/='=') $ l3
    p2  = readInt . takeWhile isDigit . dropWhile1 (/='=') . dropWhile1 (/='=') $ l3


-- Solve a single machine, based on the general solution of a 2d system
-- of linear equations. A solution can be fractional, in which case it
-- will get rounded to an integer. Thus, we need to verify every solution.
-- That's somewhat annoying, and I'm sure there are more elegant solutions
-- e.g. using some fractional data type, but good enough is good enough.
solveMachine :: ((Int, Int, Int, Int), (Int, Int)) -> Maybe (Int, Int)
solveMachine ((a11, a12, a21, a22), (p1, p2))
    | a12 * a21 - a11 * a22 == 0 = Nothing
    | a > 100 || b > 100         = Nothing
    | a * a11 + b * a12 /= p1    = Nothing
    | a * a21 + b * a22 /= p2    = Nothing
    | otherwise                  = Just (a, b)
      where
        a = - (a22 * p1 - a12 * p2) `div` (a12 * a21 - a11 * a22)
        b = - (a11 * p2 - a21 * p1) `div` (a12 * a21 - a11 * a22)


-- Calculate the cost for a single solution
cost :: Maybe (Int, Int) -> Int
cost Nothing       = 0
cost (Just (a, b)) = 3 * a + 1 * b


----------------
-- For Part 2 --
----------------

parseMachine' :: [String] -> Machine
parseMachine' (l1:l2:l3:[]) = ((a11, a12, a21, a22), (p1, p2))
  where
    a11 = readInt . takeWhile isDigit . dropWhile1 (/='+') $ l1
    a21 = readInt . takeWhile isDigit . dropWhile1 (/='+') . dropWhile1 (/='+') $ l1
    a12 = readInt . takeWhile isDigit . dropWhile1 (/='+') $ l2
    a22 = readInt . takeWhile isDigit . dropWhile1 (/='+') . dropWhile1 (/='+') $ l2
    p1  = (+10000000000000) . readInt . takeWhile isDigit . dropWhile1 (/='=') $ l3
    p2  = (+10000000000000) . readInt . takeWhile isDigit . dropWhile1 (/='=') . dropWhile1 (/='=') $ l3


solveMachine' :: ((Int, Int, Int, Int), (Int, Int)) -> Maybe (Int, Int)
solveMachine' ((a11, a12, a21, a22), (p1, p2))
    | a12 * a21 - a11 * a22 == 0 = Nothing
    | a * a11 + b * a12 /= p1    = Nothing
    | a * a21 + b * a22 /= p2    = Nothing
    | otherwise                  = Just (a, b)
      where
        a = - (a22 * p1 - a12 * p2) `div` (a12 * a21 - a11 * a22)
        b = - (a11 * p2 - a21 * p1) `div` (a12 * a21 - a11 * a22)





-- < 39684

main :: IO ()
main = do
    fileContents <- readFile "input.txt"

    let machines = map parseMachine . tok [""] . lines $ fileContents
    print $ sum
          . map (cost . solveMachine)
          $ machines

    let machines' = map parseMachine' . tok [""] . lines $ fileContents
    print $ sum
          . map (cost . solveMachine')
          $ machines'
    print $ "Done."

