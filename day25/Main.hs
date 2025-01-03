module Main where

import Data.List (transpose)
import Utils (tok)

type Lock = [Int]
type Key  = [Int]

parseInput :: String -> ([Lock], [Key])
parseInput str = (locks, keys)
  where
    blocks = tok [""] . lines $ str
    locks  = map (parse . (drop 1)) . filter isLock $ blocks
    keys   = map (parse . (take 6)) . filter isKey  $ blocks
    parse  = map (length . filter (=='#')) . transpose
    isLock = (=="#####") . head
    isKey  = (=="#####") . last

fits :: Lock -> Key -> Bool
fits lock key = all (<=5) $ zipWith (+) lock key

main :: IO ()
main = do
    (locks, keys) <- parseInput <$> readFile "input.txt"

    print $ length
          $ [1 | lock <- locks,
                 key  <- keys,
                 lock `fits` key]


    print $ "Done."
