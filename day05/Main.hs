module Main where

import Data.List (elemIndex)
import Utils (readInt, tok, tuplify2)

-- Some sugar and parsing of input

type Rule   = (Int, Int)
type Update = [Int]

parseRule :: String -> Rule
parseRule = tuplify2
          . map readInt
          . tok "|"

parseUpdate :: String -> Update
parseUpdate = map readInt
            . tok ","


-- Check whether a given rule is fulfilled by a given update.
checkRule :: Rule -> Update -> Bool
checkRule (e1, e2) u = case (elemIndex e1 u, elemIndex e2 u) of
    (Nothing, _)     -> True
    (_, Nothing)     -> True
    (Just i, Just j) -> i < j


-- Check whether all given rules are all fulfilled by a given update.
checkAllRules :: [Rule] -> Update -> Bool
checkAllRules rules update = all (\ rule -> checkRule rule update) rules


-- Get the middle element of a list.
middle :: [a] -> a
middle xs = xs !! (((length xs) - 1) `div` 2)


-- Part 1: Find all correct rules and add their middle elements
part1 :: [Rule] -> [Update] -> Int
part1 rules updates = sum
                    . map middle
                    . filter (checkAllRules rules)
                    $ updates

main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let input   = tok [""] . lines $ fileContents
    let rules   = map parseRule $ input !! 0
    let updates = map parseUpdate $ input !! 1

    print $ part1 rules updates

    print $ "Done."

