module Main where

import Utils (map2, readInt, removeAt, tok)

-- Rolling differences for a list of numbers
diffs :: Num a => [a] -> [a]
diffs []         = []
diffs (x:[])     = []
diffs (x1:x2:xs) = (x2 - x1) : diffs (x2:xs)


-- Check whether a list is "safe", i.e.
-- all differences are either in [-3; -1] or in [1; 3].
safe :: [Int] -> Bool
safe l = all (`elem` [1, 2, 3])    (diffs l) ||
         all (`elem` [-1, -2, -3]) (diffs l)


-- Part 2: Given a list, generate all lists where exactly
-- one element of the given list is left out.
skips :: [a] -> [[a]]
skips xs = map (\ i -> removeAt i xs)
         $ [0 .. (length xs) - 1]


-- Part 2: Check whether a list is "safe" based on the new
-- conditions, i.e. if either the list itself is safe, or
-- any of its "leave-one-out lists" is safe.
safe' :: [Int] -> Bool
safe' l = safe l || any safe (skips l)


main :: IO ()
main = do
    -- Read and parse file to a list of lists of ints
    fileContents <- readFile "input.txt"
    let input = map2 readInt
              . map (tok " ")
              . lines
              $ fileContents

    -- Part 1
    print $ length $ filter safe input

    -- Part 2
    print $ length $ filter safe' input

    print $ "Done."

