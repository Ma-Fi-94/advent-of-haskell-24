module Main where

import Data.List ((\\), find, sortOn)
import Data.Maybe (fromJust)
import Grid
import Utils (map2, readInt, tok, tuplify2)

import Debug.Trace (trace)

-----------
-- Setup --
-----------

-- We automatically set the position of the
-- grid's border. 6 for example, 70 for
-- actual input.
borderPos :: Int
borderPos = 70

-- We also need to hardcode start and end point.
-- Start point is always (0, 0), end point is
-- at the opposite corner.
startCoord, endCoord :: Coord
(startCoord, endCoord) = ((0, 0), (borderPos, borderPos))


-- Parse the input into a list of coordinates which
-- are non-walkable due to being corrupted.
parseCorrupted :: String -> [Coord]
parseCorrupted = map tuplify2
               . map2 readInt
               . map (tok ",")
               . lines


------------------------------------
-- Stuff for Dijkstra's algorithm --
------------------------------------

data DijkstraRecord = D {
  state       :: Coord,
  predecessor :: Coord,
  distance    :: Int,
  visited     :: Bool
} deriving (Show, Eq)


-- Dirty little hacks
infinity :: Int
infinity = 1_000_000_000_000

nilCoord :: Coord
nilCoord = (-1, -1)


-- Helper to find all possible next positions given a current one.
-- Every step has a cost of 1.0, so we just return the steps
-- without cost.
nextStates :: Coord -> [Coord]
nextStates (r, c) = [(r, c + 1), (r, c - 1), (r - 1, c), (r + 1, c)]


---------------------
-- Dijkstra itself --
---------------------

-- Run Dijkstra's algorithm until all nodes have been visited. For part 2,
-- we cannot uphold our assumption that all cells will always be reachable,
-- hence, in the second guard we now introduced an additional stopping
-- criterion: if all unvisited records have infinite distance (i.e. are not
-- reachable), we stop as well.
dijkstra :: [DijkstraRecord] -> [DijkstraRecord]
dijkstra records
    | null unvisitedRecords                          = records
    | all ((==infinity) . distance) unvisitedRecords = records
    | otherwise                                      = trace ("Remaining number of records: " ++ show (length unvisitedRecords)) $  dijkstra (visitedRecords' ++ unvisitedRecords')
  where
    -- Identify all visited records, i.e. those we won't change anymore.
    -- We separate them for an easier recursive function call later.
    visitedRecords   = filter visited records

    -- Identify all records we still have to visit, and their states.
    -- Some of these will change during the current step.
    unvisitedRecords = filter (not . visited) records
    unvisitedStates  = map state unvisitedRecords

    -- We work on the unvisited record with the smallest distance.
    currentRecord    = head . sortOn distance $ unvisitedRecords
    currentState     = state currentRecord
    currentDistance  = distance currentRecord

    -- These are all next steps we can take, as well as their cumulative
    -- distances when stepping through the current record.
    nextOptions = map (\ state -> (state, currentDistance + 1))
                . filter (\ state -> state `elem` unvisitedStates)
                $ nextStates currentState

    -- Now we step through all unvisited records. First, we remove the
    -- current record, then for any record, we update distance and
    -- predecessor, if we can reach it faster through the current record.
    unvisitedRecords' = map (\ record -> if (state record) `elem` (map fst nextOptions)
                                             && (distance record) > snd ( fromJust ( find ((== state record) . fst) nextOptions))
                                         then D {state       = state record,
                                                 predecessor = currentState,
                                                 distance    = snd ( fromJust ( find ((== state record) . fst) nextOptions)),
                                                 visited     = False}
                                         else record)
                      . filter (/= currentRecord)
                      $ unvisitedRecords

    -- Update current record by marking it as visited
    currentRecord'   = D {state       = state currentRecord,
                          predecessor = predecessor currentRecord,
                          distance    = distance currentRecord,
                          visited     = True}

    -- And add it to the visited records
    visitedRecords' = currentRecord' : visitedRecords










main :: IO ()
main = do
    fileContents       <- readFile "input.txt"
    let allCoords       = [(r, c) | r <- [0..borderPos],
                                    c <- [0..borderPos]]
    let corruptedCoords = parseCorrupted fileContents

    ------------
    -- Part 2 --
    ------------

    -- Key idea here is to do a binary search on the input list.
    -- We do know that 1024 is still fine, and the list has 3450
    -- elements altogether.
    -- 2500 works.
    -- 3000 works
    -- 3500 does not work
    -- 3250 does not work
    -- 3100 does not work
    -- 3050 does not work
    -- 3025 works
    -- 3040 does not work
    -- 3030 works
    -- 3035 works
    -- 3037 works
    -- 3038 does not work.

    let walkableCoords = allCoords \\ (take 3038 corruptedCoords)

    -- Prepare for executing Dijkstra
    let startRecord  = D {state       = startCoord,
                          predecessor = nilCoord,
                          distance    = 0,
                          visited     = False}
    let otherRecords = map (\ coord -> D {state       = coord,
                                          predecessor = nilCoord,
                                          distance    = infinity,
                                          visited     = False})
                       (walkableCoords \\ [startCoord])
    let allRecords   = startRecord : otherRecords

    -- Execute Dijkstra
    let result = dijkstra allRecords

    -- Find length of the shortest path to the finish
    print $ minimum
          . map distance
          . filter ((==endCoord) . state)
          $ result

    -- Accordingly, the 3038th coordinate is the culprit, which has index 3037:
    print $ corruptedCoords !! 3037




    print $ "Done."

