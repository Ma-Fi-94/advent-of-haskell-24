module Main where

import Data.List (intercalate, sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (fastNub, tok)

import Debug.Trace (trace)

-- We define Edge as Sets with two elements for easy querying.
type Node  = String
type Edge  = Set Node
type Graph = Set Edge

-- Parse the input to the Graph structure defined above.
parseInput :: String -> Graph
parseInput = Set.fromList
           . map Set.fromList
           . map (tok "-")
           . lines

-- Extract all nodes from a graph.
getNodes :: Graph -> [Node]
getNodes = fastNub
         . concatMap Set.toList
         . Set.toList

-- Query the graph and check whether two nodes are connected.
connected :: Graph -> Node -> Node -> Bool
connected graph node1 node2 = (Set.fromList [node1, node2]) `Set.member` graph

-- `nub` for a list of cliques using Sets.
nubCliques :: [[Node]] -> [[Node]]
nubCliques = map Set.toList . Set.toList . Set.fromList . map Set.fromList


------------
-- Part 2 --
------------

-- Given an n-clique, check whether it is also part of (n+1)-cliques.
-- If so, return the (n+1)-cliques.
step :: Graph -> [Node] -> [[Node]]
step graph clique = [newNode : clique | newNode <- getNodes graph,
                                        newNode `notElem` clique,
                                        all (connected graph newNode) clique]

-- Search the largest clique (which we assume to be unique), given the graph
-- and some starting cliqueS -- once we're down to exactly one clique, we
-- terminate.
searchLargest :: Graph -> [[Node]] -> [Node]
searchLargest graph cliques
    | length cliques == 1 = head cliques
    | otherwise           = trace ("Current number of cliques: " ++ show (length cliques)) searchLargest graph $ (nubCliques (concatMap (step graph) cliques))


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let graph     = parseInput fileContents
    let nodes      = getNodes graph

    ------------
    -- Part 1 --
    ------------

    let candidates = filter (\ s -> (s !! 0) == 't') nodes


    print $ length
          . nubCliques
          $ [[candidate, node1, node2] | candidate <- candidates,
                                         node1     <- nodes,
                                         node2     <- nodes,
                                         node1     /= node2,
                                         connected graph candidate node1,
                                         connected graph candidate node2,
                                         connected graph node1 node2]


    ------------
    -- Part 2 --
    ------------

    print $ intercalate ","
          . sort
          . searchLargest graph
          $ [[n] | n <- nodes]


    print $ "Done."

