module Main where


import Data.Function (on)
import Data.List (intercalate, sort, sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (readInt, tok)


-- Sugar, ah honey, honey... 
type Knowns     = Map String Bool
type Unknowns   = Map String (String, String, (Bool -> Bool -> Bool))


-- Parsing as always. Nothing fancy here.
parseInput :: String -> (Knowns, Unknowns)
parseInput input = (knowns, unknowns)
  where
    blocks         = tok [""] . lines $ input
    knowns         = Map.fromList $ map parseKnown (blocks !! 0)
    unknowns       = Map.fromList $ map parseUnknown (blocks !! 1)
    parseKnown s   = (name, bool)
      where
        name = (!!0) . tok ": " $ s
        bool = toEnum . readInt . (!!1) . tok ": " $ s
    parseUnknown s = (name, (operand1, operand2, f))
      where
        name      = (!!3) . tok " ->" $ s
        operand1  = (!!0) . tok " ->" $ s
        operand2  = (!!2) . tok " ->" $ s
        operation = (!!1) . tok " ->" $ s 
        f         = case operation of
            "AND" -> (&&)
            "OR"  -> (||)
            "XOR" -> (/=)


-- Solve all the unknowns.
solve :: (Knowns, Unknowns) -> Knowns
solve (knowns, unknowns)
    | Map.null unknowns = knowns
    | otherwise         = solve (knowns', unknowns')
  where
    unknowns' = Map.delete (fst current) unknowns
    knowns'   = Map.insert (fst current) (snd current) knowns
    current   = (\ (name, (opd1, opd2, op))
                     -> (name, (knowns Map.! opd1) `op` (knowns Map.! opd2)))
              . head
              . filter (\ (name, (opd1, opd2, op))
                            -> all (`Map.member` knowns) [opd1, opd2])
              $ Map.assocs unknowns


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let (knowns, unknowns) = parseInput fileContents


    -- Part 1: Solve all and extract the z-wires,
    -- then convert to decimal.
    print . sum
          . zipWith (*) [2 ^ i | i <- [0..]]
          . map (fromEnum . snd)
          . sortBy (compare `on` fst)
          . filter ((=='z') . (!!0) . fst)
          . Map.assocs
          $ solve (knowns, unknowns)

    -- Part 2: I manually made a table for every adder, tracking
    -- input, output and inner wires. In my case, problems
    -- occured at bits 18, 27, 31, and 39. Sorting the affected
    -- gates and writing them as a single, comma-separated
    -- string yields the answer.
    print $ intercalate ","
          $ sort ["hmt", "z18", "bfq", "z27", "z31", "hkh", "fjp", "bng"]

    print $ "Done."

