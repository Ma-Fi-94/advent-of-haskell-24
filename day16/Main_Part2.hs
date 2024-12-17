{-# LANGUAGE LambdaCase, NumericUnderscores #-}

module Main where

import Data.List (find, sortOn)
import Data.Maybe (fromJust)
import Grid (Coord)
import qualified Grid as G
import Data.Set (Set)
import qualified Data.Set as Set

import Debug.Trace (trace)

-- Sugar
type Start       = Coord
type Finish      = Coord
type Orientation = Char
type State       = (Coord, Orientation)
type Cost        = Int
type Path        = [DijkstraRecord]

data DijkstraRecord = D {
  state        :: State,
  predecessors :: [State],
  distance     :: Int,
  visited      :: Bool
} deriving (Show, Eq)


-- Dirty little hacks
infinity :: Int
infinity = 1_000_000_000_000

-- Helper for turning clockwise given a current direction.
-- We could use a custom cyclic data type here, but that
-- seems kinda overkill.
turnClockwise :: Orientation -> Orientation
turnClockwise = \case
                  'N' -> 'E'
                  'E' -> 'S'
                  'S' -> 'W'
                  'W' -> 'N'


-- Helper for turning anticlockwise given a current direction.
turnAntiClockwise :: Orientation -> Orientation
turnAntiClockwise = \case
                      'N' -> 'W'
                      'W' -> 'S'
                      'S' -> 'E'
                      'E' -> 'N'


-- Helper to find all possible next states given a current one,
-- as well as the distance ("score") to reach each of them.
-- We can either take one step forward in the current orientation,
-- or turn clockwise, or turn anticlockwise.
nextStates :: State -> [(State, Cost)]
nextStates ((row, column), orientation) = [(forward, 1),
                                           (clockwise, 1_000),
                                           (anticlockwise, 1_000)]
  where
    forward       = case orientation of
        'N' -> ((row - 1, column), orientation)
        'S' -> ((row + 1, column), orientation)
        'W' -> ((row, column - 1), orientation)
        'E' -> ((row, column + 1), orientation)
    clockwise     = ((row, column), turnClockwise orientation)
    anticlockwise = ((row, column), turnAntiClockwise orientation)
    

-- Does exactly what it says on the tin
parseInput :: [String] -> (Start, Finish, [State])
parseInput input = (start, finish, states)
  where
    cells  = G.enumerate
           . G.fromList
           $ input
    start  = head
           . map fst
           . filter (\ (_, char) -> char == 'S')
           $ cells
    finish = head
           . map fst
           . filter (\ (_, char) -> char == 'E')
           $ cells
    states = concatMap (\ coord -> [(coord, orientation) | orientation <- "NSWE"])
           . map fst
           . filter (\ (_, char) -> char `elem` ".SE")
           $ cells


-- Dijkstra like in Part 1, but we now keep track of all possible shortest paths.
dijkstra :: [DijkstraRecord] -> [DijkstraRecord]
dijkstra records
    | null unvisitedRecords = records
    | otherwise             = trace ("Remaining number of records: " ++ show (length unvisitedRecords)) $  dijkstra (visitedRecords' ++ unvisitedRecords')
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
    nextOptions = map (\ (state, stepDist) -> (state, stepDist + currentDistance))
                . filter (\ (state, stepDist) -> state `elem` unvisitedStates)
                $ nextStates currentState

    -- Now we step through all unvisited records. First, we remove the
    -- current record. Then, for every record we apply an update, depending
    -- on whether we can reach the respective record through the current
    -- one, and, if so, with a strictly lesser distance or the same distance
    -- the respective record has currently assigned.
    unvisitedRecords' = map update
                      . filter (/= currentRecord)
                      $ unvisitedRecords
      where
        update record
            | state record `notElem` map fst nextOptions = record
            | snd ( fromJust ( find ((== state record) . fst) nextOptions)) < distance record = D {state        = state record,
                                                                                                   predecessors = [currentState],
                                                                                                   distance     = snd ( fromJust ( find ((== state record) . fst) nextOptions)),
                                                                                                   visited      = False}
            | snd ( fromJust ( find ((== state record) . fst) nextOptions)) == distance record = D {state        = state record,
                                                                                                    predecessors = currentState : (predecessors record),
                                                                                                    distance     = distance record,
                                                                                                    visited      = False}
            | otherwise = record


    -- Update current record by marking it as visited
    currentRecord'   = D {state        = state currentRecord,
                          predecessors = predecessors currentRecord,
                          distance     = distance currentRecord,
                          visited      = True}

    -- And add it to the visited records
    visitedRecords' = currentRecord' : visitedRecords


-- Given a terminal record (with minimum distance), we trace back
-- all possible paths until they all reach the given starting record.
traceBackTo :: DijkstraRecord -> [DijkstraRecord] -> [Path] -> [Path]
traceBackTo startRecord dijkstraResult paths
    | all (null . predecessors . last) paths = paths
    | otherwise                              = traceBackTo startRecord dijkstraResult paths'
  where
    paths' = concatMap (\ path -> if   null . predecessors . last $ path
                                  then [path]
                                  else [path ++ [predecessorRecord] |
                                            predecessorState <- predecessors (last path),
                                            let predecessorRecord = head ( filter ((==predecessorState) . state) dijkstraResult )
                                        ])
           $ paths


main :: IO ()
main = do
    -- Read and parse input
    fileContents                         <- readFile "input.txt"
    let (startCoord, finishCoord, states) = parseInput . lines $ fileContents

    -- Preparing for Dijkstra's algorithm
    let startState      = (startCoord, 'E')
    let otherStates     = filter (/= startState)
                        $ states

    let startRecord     = D {state        = startState,
                             predecessors = [],
                             distance     = 0,
                             visited      = False}
    let otherRecords    = map (\ state -> D {state        = state,
                                             predecessors = [],
                                             distance     = infinity,
                                             visited      = False})
                              otherStates
    let allRecords      = startRecord : otherRecords

    -- Execute for Dijkstra
    let result = dijkstra allRecords

    -- Analyse result. By inspection, I found that there is exactly one optimum
    -- terminal record, so we take the head of the list. We extract all optimal
    -- paths.
    let allTerminalRecords = filter ((==finishCoord) . fst . state) result
    let minDistance        = minimum . map distance $ allTerminalRecords
    let minTerminalRecord  = head $ filter ((==minDistance) . distance) allTerminalRecords
    let optimalPaths       = traceBackTo startRecord result [[minTerminalRecord]]

    -- For every optimal path, we only care about the coordinates of its records.
    let trajectories = map (map (fst . state))
                     $ optimalPaths

    -- Now we only need to find all coordinates that occur at least once
    print $ Set.size
          . Set.fromList
          . concat
          $ trajectories



    print $ "Done."

