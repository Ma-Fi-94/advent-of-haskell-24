module Main where

import Data.Bits (xor)
import Data.Map (Map)
import qualified Data.Map as Map


-- Sugar
type Secret = Int
type Price  = Int


parseInput :: String -> [Secret]
parseInput = map read . lines


stepSecret :: Secret -> Secret
stepSecret secret = step3
  where
    step3 = prune $ mix step2  (2048 * step2)
    step2 = prune $ mix step1  (step1 `div` 32)
    step1 = prune $ mix secret (64 * secret)
    prune = (`rem` 16777216)
    mix   = xor


------------
-- Part 2 --
------------

-- For every monkey, we construct a Map which maps a 4-tuple of Ints
-- (the price changes) onto the corresponding price.
-- We fill the Map in reverse order because only the first occurence
-- of any 4-tuple counts.
makeMap :: Secret -> Map (Price, Price, Price, Price) Price
makeMap secret = Map.fromList
               . reverse
               $ go changes (drop 4 prices)
  where
    prices           = map (`rem` 10) . take 2001 $ iterate stepSecret secret
    changes          = zipWith (-) (tail prices) prices
    go _ []          = []
    go (c:cs) (p:ps) = ((c, cs !! 0, cs !! 1, cs !! 2), p)
                     : go cs ps



main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let secrets  = parseInput $ fileContents

    -- Part 1: Trivial calculation.
    print $ sum
          . map ((!!2000) . iterate stepSecret)
          $ secrets

    -- Part 2: For every monkey, we make a Map which
    -- assigns a 4-tuple of price changes to its price.
    -- We then just need to merge the Maps via unionWith (+)
    -- and identify the maximum.
    let monkeyMaps = map makeMap secrets
    let mergedMap  = Map.unionsWith (+) monkeyMaps
    print          $ maximum
                   . map snd
                   . Map.assocs
                   $ mergedMap


    print $ "Done."

