module Main where

import Utils (at, readInt, tok)

type Line = (Int, [Int])

-- Parsing of a single line of the input file.
parseLine :: String -> Line
parseLine line = (target, values)
  where
    target = readInt . at 0 . tok ":" $ line
    values = map readInt . tok " " . at 1 . tok ":" $ line


-- Check whether a given entry is solvable (Part 1)
solvable :: Line -> Bool
solvable (target, values) = go (head values) (tail values)
  where
    go accum remaining
        | accum > target                    = False
        | null remaining && accum /= target = False
        | null remaining && accum == target = True
        | otherwise                         = added || multiplied
          where
            curValue   = head remaining
            remaining' = tail remaining
            added      = go (accum + curValue) remaining'
            multiplied = go (accum * curValue) remaining'


-- Check whether a given entry is solvable (Part 2)
solvable' :: Line -> Bool
solvable' (target, values) = go (head values) (tail values)
  where
    go accum remaining
        | accum > target                    = False
        | null remaining && accum /= target = False
        | null remaining && accum == target = True
        | otherwise                         = added || multiplied || concated
          where
            curValue   = head remaining
            remaining' = tail remaining
            added      = go (accum + curValue) remaining'
            multiplied = go (accum * curValue) remaining'
            concated   = go (readInt (show accum ++ show curValue)) remaining'

-- It may be noted that there is an obvious generalisation of `solvable`,
-- where we provide a list of arbitrary binary operations which is mapped
-- over in the recursive call of `go`.

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let input = map parseLine . lines $ fileContents

    print $ sum
          . map fst
          . filter solvable
          $ input

    print $ sum
          . map fst
          . filter solvable'
          $ input

    print $ "Done."

