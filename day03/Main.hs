module Main where

import Data.Char (isDigit)
import Text.Regex.TDFA
import Utils (dropWhile1, readInt)

-- Extract all proper mul() statements.
-- We don't need to do any monadic shenanigans here,
-- because the list monad takes care of potentials fails.
-- We also don't need explicit type annotation for
-- getAllTextMatches, since the desired type can be inferred.
findMuls :: String -> [String]
findMuls s = getAllTextMatches (s =~ regex)
  where
  	regex = "mul\\([0-9]+,[0-9]+\\)"


-- Given a proper mul() statement, evaluate it.
evalMul :: String -> Int
evalMul s = a * b
  where
  	a = readInt . takeWhile isDigit . drop 4 $ s
  	b = readInt . takeWhile isDigit . dropWhile1 (/=',') $ s


------------
-- Part 2 --
------------

-- As findMuls, but we also find "do()" and "don't()".
findCommands :: String -> [String]
findCommands s = getAllTextMatches (s =~ regex)
  where
  	regex = "mul\\([0-9]+,[0-9]+\\)|do\\(\\)|don't\\(\\)"


-- Given a list of commands, remove mul()s that are
-- inactive due to a don't().
removeInactives :: [String] -> [String]
removeInactives = go True
  where
  	go _ []             = []
  	go _ ("do()":ss)    = go True ss
  	go _ ("don't()":ss) = go False ss
  	go False (s:ss)     = go False ss
  	go True (s:ss)      = s : go True ss


main :: IO ()
main = do
    fileContents <- readFile "input.txt"

    print $ sum
          . map evalMul
          . findMuls
          $ fileContents

    print $ sum
          . map evalMul
          . removeInactives
          . findCommands
          $ fileContents

    print $ "Done."
