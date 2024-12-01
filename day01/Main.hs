module Main where

import Data.List (sort)
import Utils (map2, readInt, tok)

main :: IO ()
main = do
    -- Read and parse file to a list of lists of ints
    fileContents <- readFile "input.txt"
    let input = map2 readInt
              . map (tok " ")
              . lines
              $ fileContents

    -- Left and right column of the input
    let left  = map (!!0) $ input
    let right = map (!!1) $ input

    -- Part 1: Sum of absolute differences of sorted column elements
    print $ sum
          . map (abs)
          $ zipWith (-) (sort left) (sort right)

    -- Part 2: The similarity score, as defined in the text
    print $ sum
          . map (\ i -> i * length (filter (==i) right) )
          $ left

    print $ "Done."

