{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List (sortBy)
import Data.Function (on)
import Utils (readInt)

-- Sugar
type Button = Char
type Coord  = (Int, Int)


-- For the first keypad, convert a button to its coordinate
-- position on the keypad
buttonToCoord :: Button -> Coord
buttonToCoord = \case {'7' -> (0, 0); '8' -> (0, 1); '9' -> (0, 2);
                       '4' -> (1, 0); '5' -> (1, 1); '6' -> (1, 2);
                       '1' -> (2, 0); '2' -> (2, 1); '3' -> (2, 2);
                                      '0' -> (3, 1); 'A' -> (3, 2)}


-- For the other keypads, convert a button to its coordinate
-- position on the keypad
buttonToCoord' :: Button -> Coord
buttonToCoord' = \case {               '^' -> (0, 1); 'A' -> (0, 2);
                        '<' -> (1, 0); 'v' -> (1, 1); '>' -> (1, 2)}


-- For the first keypad, find the at most two possible optimum segments
-- between two given buttons.
findSegment :: Button -> Button -> [[Button]]
findSegment b1 b2 = run (buttonToCoord b1) (buttonToCoord b2)
  where
    run (r1, c1) (r2, c2)
        | (r1, c1) == (r2, c2) = ["A"]
        | r1 == r2             = [hor ++ "A"]
        | c1 == c2             = [ver ++ "A"]
        | (c1 == 0 && r2 == 3) = [horVer ++ "A"]
        | (r1 == 3 && c2 == 0) = [verHor ++ "A"]
        | otherwise            = [horVer ++ "A", verHor ++ "A"]
          where
            horVer = hor ++ ver
            verHor = ver ++ hor
            hor    = if   c2 >= c1
                     then replicate (c2 - c1) '>'
                     else replicate (c1 - c2) '<'
            ver    = if   r2 >= r1
                     then replicate (r2 - r1) 'v'
                     else replicate (r1 - r2) '^'


-- For the other keypads, find the at most two possible optimum segments
-- between two given buttons.
findSegment' :: Button -> Button -> [[Button]]
findSegment' b1 b2 = run (buttonToCoord' b1) (buttonToCoord' b2)
  where
    run (r1, c1) (r2, c2)
        | (r1, c1) == (r2, c2) = ["A"]
        | r1 == r2             = [hor ++ "A"]
        | c1 == c2             = [ver ++ "A"]
        | c1 == 0 && r2 == 0   = [horVer ++ "A"]
        | r1 == 0 && c2 == 0   = [verHor ++ "A"]
        | otherwise            = [horVer ++ "A", verHor ++ "A"]
          where
            horVer = hor ++ ver
            verHor = ver ++ hor
            hor    = if   c2 >= c1
                     then replicate (c2 - c1) '>'
                     else replicate (c1 - c2) '<'
            ver    = if   r2 >= r1
                     then replicate (r2 - r1) 'v'
                     else replicate (r1 - r2) '^'


-- For the first keypad, find all possible lists of commands to
-- key in an entire sequence of given characters. We assume that
-- we start on the 'A' key, as per the task.
findPath :: [Button] -> [[Button]]
findPath (button:buttons) = go (findSegment 'A' button)
                               button
                               (head buttons)
                               (drop 1 buttons)
  where
    go accum current next rest
        | rest == [] = accum'
        | otherwise  = go accum' next (head rest) (drop 1 rest)
          where
            accum' = concatMap
                     (\path -> [path ++ segment
                                   | segment <- findSegment current next])
                     $ accum


-- For the other keypads, find all possible lists of commands to
-- key in an entire sequence of given characters. We assume that
-- we start on the 'A' key, as per the task.
findPath' :: [Button] -> [[Button]]
findPath' (button:buttons) = go (findSegment' 'A' button)
                                 button
                                 (head buttons)
                                 (drop 1 buttons)
  where
    go accum current next rest
        | rest == [] = accum'
        | otherwise  = go accum' next (head rest) (drop 1 rest)
          where
            accum' = concatMap
                     (\path -> [path ++ segment
                                   | segment <- findSegment' current next])
                     $ accum


-- Part 1: Calculate the complexity of a code
--complexity :: [Button] -> Int
complexity code = shortestLength -- * numericPart
  where
    shortestLength = id -- minimum
                   $ map length
                   . concatMap findPath'
                   . concatMap findPath'
                   $ findPath code
    numericPart    = readInt
                   $ take 3 code


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let codes = lines fileContents

    print $ id -- sum 
          . map complexity
          $ codes


    print $ "Done."

