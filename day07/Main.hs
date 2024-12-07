module Main where

import Utils (at, readInt, tok)

type Line            = (Int, [Int])
type BinaryOperation = Int -> Int -> Int

-- Parsing of a single line of the input file.
parseLine :: String -> Line
parseLine line = (target, values)
  where
    target = readInt . at 0 . tok ":" $ line
    values = map readInt . tok " " . at 1 . tok ":" $ line


-- Check whether a given line is solvable using a list of given
-- binary operations. Due to our "early stopping", this solver
-- only works for operations ⊕ which satisfy a, b < a ⊕ b.
solvableWith :: [BinaryOperation] -> Line  -> Bool
solvableWith operations (target, values) = go (head values) (tail values)
  where
    go accum remaining
        | accum > target                    = False
        | null remaining && accum /= target = False
        | null remaining && accum == target = True
        | otherwise                         = any tryOperation operations
          where
            tryOperation = (\ op -> go (accum `op` curValue) remaining')
            curValue     = head remaining
            remaining'   = tail remaining


-- For Part 2, we define the concatInt operation as (|||), since (||) is
-- already taken.
(|||) :: Int -> Int -> Int
(|||) i1 i2 = readInt $ show i1 ++ show i2


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let input = map parseLine . lines $ fileContents

    print $ sum
          . map fst
          . filter (solvableWith [(+), (*)])
          $ input

    print $ sum
          . map fst
          . filter (solvableWith [(+), (*), (|||)])
          $ input

    print $ "Done."

