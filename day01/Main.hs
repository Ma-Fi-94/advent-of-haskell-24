module Main where

main :: IO ()
main = do
    fileContents <- readFile "input.txt"

    print $ "Done."

