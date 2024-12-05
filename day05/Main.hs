module Main where

import Data.List (elemIndex, sortBy)
import Utils (readInt, tok, tuplify2)

-- Some sugar and parsing of input

type Rule   = (Int, Int)
type Update = [Int]

parseInput :: String -> ([Rule], [Update])
parseInput x = (rules, updates)
  where
    blocks  = tok [""] . lines $ x
    rules   = map parseRule $ blocks !! 0
    updates = map parseUpdate $ blocks !! 1


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
middle xs = xs !! (length xs `div` 2)


-- Part 1: Find all correct updates and add their middle elements
part1 :: [Rule] -> [Update] -> Int
part1 rules updates = sum
                    . map middle
                    . filter (checkAllRules rules)
                    $ updates

-- Part 2 --

-- Comparison function based on the provided ruleset.
-- We will use this for sorting malformed updates.
comp :: [Rule] -> Int -> Int -> Ordering
comp rules e1 e2
    | (e1, e2) `elem` rules = LT
    | (e2, e1) `elem` rules = GT
    | otherwise             = EQ


-- Given all rules and a defective update, fix it by sorting it.
fixUpdate :: [Rule] -> Update -> Update
fixUpdate rules update = sortBy (comp rules) update


-- Find all incorrect updates, fix them, and add their middle elements.
part2 :: [Rule] -> [Update] -> Int
part2 rules updates = sum
                    . map middle
                    . map (fixUpdate rules)
                    . filter (not . checkAllRules rules)
                    $ updates


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let (rules, updates) = parseInput fileContents

    print $ part1 rules updates
    print $ part2 rules updates

    print $ "Done."
