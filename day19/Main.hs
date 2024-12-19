module Main where

import Data.Function (on)
import Data.List (isPrefixOf, nubBy)
import Utils (tok, snd3, thi3, nubByWith)
import Debug.Trace (trace)

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
                                  . nubBy ((==) `on` snd)
                                  . map (\ (towel, prefix, rest) ->
                                        (prefix ++ towel, drop (length towel) rest))
                                  $ [(towel, prefix, rest)
                                        | towel           <- towels,
                                          (prefix, rest)  <- parses,
                                          towel `isPrefixOf` rest]


main :: IO ()
main = do
    fileContents          <- readFile "input.txt"
    let (towels, designs) = parseInput fileContents

    print $ length
          . filter (isPossible towels)
          $ designs

    print $ "Done."

