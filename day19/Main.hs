module Main where

import Data.Function (on)
import Data.List (isPrefixOf, nubBy)
import Utils (tok, fst3, snd3, thi3, nubByWith)

-- Sugar
type Towel  = String
type Design = String


-- Does exactly what it says on the tin
parseInput :: String -> ([Towel], [Design])
parseInput input = (towels, designs)
  where
    blocks  = tok [""] . lines $ input
    towels  = tok ", " . head $ blocks !! 0
    designs = blocks !! 1


-- Check whether the given towels can produce a given design.
-- We keep track of all attempts to construct a design by having
-- a list of 2-tuples, where the first tuple element contains the
-- found prefix so far, and the second tuple element contains
-- the rest, which has not been parsed so far.
isPossible :: [Towel] -> Design -> Bool
isPossible towels design = go [([], design)]
  where
    go parses
        | null parses             = False
        | any (null . snd) parses = True
        | otherwise               = go
                                  . nubBy ((==) `on` fst)
                                  . map (\ (towel, prefix, rest) ->
                                        (prefix ++ towel, drop (length towel) rest))
                                  $ [(towel, prefix, rest)
                                        | towel           <- towels,
                                          (prefix, rest)  <- parses,
                                          towel `isPrefixOf` rest]




-- Count the number of different ways a given design can be produced.
-- Like before, we want to keep the lists small, so we "merge" elements
-- with the same prefix. However, every list elements carries a counter,
-- and upon merging entries, the counters have to be added.
nbPossibilities :: [Towel] -> Design -> Int
nbPossibilities towels design = go [([], design, 1)]
  where
    go parses
        | null parses              = 0
        | any (null . snd3) parses = sum (map thi3 (filter (null . snd3) parses))
                                   + go (filter (not . null . snd3) parses)
        | otherwise                = go
                                   . nubByWith
                                         ((==) `on` fst3)
                                         (\ tuples -> (fst3 (head tuples),
                                                       snd3 (head tuples),
                                                       sum (map thi3 tuples))) 
                                   . map (\ (towel, prefix, rest, count) ->
                                         (prefix ++ towel, drop (length towel) rest, count))
                                   $ [(towel, prefix, rest, count)
                                         | towel                 <- towels,
                                           (prefix, rest, count) <- parses,
                                           towel `isPrefixOf` rest] 


main :: IO ()
main = do
    fileContents          <- readFile "input.txt"
    let (towels, designs) = parseInput fileContents
    print $ length
          . filter (isPossible towels)
          $ designs

    print $ sum
          . map (nbPossibilities towels)
          $ designs

    print $ "Done."

