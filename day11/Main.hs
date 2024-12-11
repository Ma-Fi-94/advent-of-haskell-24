module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Utils (half, readInt, tok)


-- Sugar
type Stone = Int
type Count = Int
type State = Map Stone Count


-- Parse the input file into a State Map.
parseInput :: String -> State
parseInput = Map.fromListWith (+)
           . map (\ i -> (i, 1))
           . map readInt 
           . tok " "


-- One transformation step for a single given stone.
nextStones :: Stone -> [Stone]
nextStones i
    | i == 0                   = [1]
    | even . length . show $ i = map readInt . half . show $ i
    | otherwise                = [2024 * i]


-- Given a State, find the next State. The key idea here is to
-- tidy up the State at every step with fromListWith (+), so
-- that every stone type and its count only occurs once.
nextState :: State -> State
nextState = Map.fromListWith (+)
          . concatMap (\ (stone, count) -> [(nextStone, count)
                                            | nextStone <- nextStones stone])
          . Map.assocs


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let state0    = parseInput fileContents

    print $ sum
          . map snd
          . Map.assocs
          $ (iterate nextState state0) !! 25

    print $ sum
          . map snd
          . Map.assocs
          $ (iterate nextState state0) !! 75
    print $ "Done."

