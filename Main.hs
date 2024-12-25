module Main where

-- Note: To make use of parallelisation, add the following to the .cabal file:
--    ghc-options:
--        -O3
--        -threaded
--        -rtsopts
--        -with-rtsopts=-N8


main :: IO ()
main = do
    fileContents <- readFile "input.txt"

    print $ "Done."

