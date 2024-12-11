module Main where

import Utils (readInt, tok)

type Stone = Int

-- Half an even list into a list of two lists.
half :: [a] -> [[a]]
half s
    | odd (length s) = error "Main.half: List has odd number of element."
    | otherwise      = [take h s, drop h s]
      where
        h = (length s) `div` 2


-- One transformation for a single stone.
blink :: Stone -> [Stone]
blink i
    | i == 0                   = [1]
    | even . length . show $ i = map readInt . half . show $ i
    | otherwise                = [2024 * i]


-- One step for a given list of stones.
step :: [Stone] -> [Stone]
step = concatMap blink


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let input    = map readInt . tok " " $ fileContents

    print $ length $ ((iterate step input) !! 25)
    print $ "Done."

