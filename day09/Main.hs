module Main where

import Data.List (reverse, group)
import Utils (readInt)

-- Record := (index, content)
type Index   = Int
type Content = Int
type Record  = (Index, Content)


-- Helpers
empty :: Record -> Bool
empty (_, (-1)) = True
empty (_, _)    = False

full :: Record -> Bool
full = not . empty


-- Mix two lists, taking elements alternatingly.
mix :: [a] -> [a] -> [a]
mix (x:xs) (y:ys) = x : y : mix xs ys
mix x [] = x
mix [] y = y


-- Parses input string to [(Index, Content)]. Free blocks get Content = -1.
parseInput :: String -> [Record]
parseInput input = zip [0..]
                 . concatMap (\ (len, content) -> replicate len content)
                 $ zip lengths contents
  where
    lengths  = map (readInt . (:[])) input
    contents = mix [0..] $ repeat (-1)


-- The actual defragmentation. We work with two copies of the same list,
-- one of which is reversed. In this way, we habe O(1) access to the head
-- and the last element of the list. This is fast enough for us to be able
-- to naively expand the entire input to a list of inidividual memory cells.
-- An obvious optimisation would be a list of contiguous blocks with the
-- same content, but this does not seem required.
defrag :: [Record] -> [Record]
defrag xs = go xs (reverse xs)
  where
    go (f@(i1, c1):fs) (r@(i2, c2):rs)
        | i1 == i2  = [f]
        | full f    = f : go fs (r:rs)
        | empty r   = go (f:fs) rs
        | otherwise = (i1, c2) : go fs rs


-- Calculate the checksum based on a list of records (with indexes, which
-- we drop in the first step).
checksum :: [Record] -> Int
checksum = sum . map (uncurry (*))


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let input    = parseInput fileContents

    print $ checksum . defrag $ input

    print $ "Done."

