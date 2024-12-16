{-# LANGUAGE LambdaCase, NumericUnderscores #-}

module Main where

import Data.List (find, sortOn)
import Data.Maybe (fromJust)
import Grid (Coord)
import qualified Grid as G

import Debug.Trace (trace)

-- Sugar
type Start       = Coord
type Finish      = Coord
type Orientation = Char
type State       = (Coord, Orientation)
type Cost        = Int

data DijkstraRecord = D {
  state       :: State,
  predecessor :: State,
  distance    :: Int,
  visited     :: Bool
} deriving (Show, Eq)


-- Dirty little hacks
infinity :: Int
infinity = 1_000_000_000_000

nilState :: State
nilState = ((-1, -1), 'X')

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


-- Run Dijkstra's algorithm until all nodes have been visited.
-- We assume that all states are reachable, hence we can use
-- a simple stop condition in the first guard, and we can
-- assume that as long as the algo runs, there's always at
-- least one unvisited record with non-infinite distance.
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
    -- Read and parse input
    fileContents                         <- readFile "input.txt"
    let (startCoord, finishCoord, states) = parseInput . lines $ fileContents

    -- Preparing for Dijkstra's algorithm
    let startState      = (startCoord, 'E')
    let otherStates     = filter (/= startState)
                        $ states

    let startRecord     = D {state       = startState,
                             predecessor = nilState,
                             distance    = 0,
                             visited     = False}
    let otherRecords    = map (\ state -> D {state       = state,
                                             predecessor = nilState,
                                             distance    = infinity,
                                             visited     = False})
                              otherStates
    let allRecords      = startRecord : otherRecords

    -- Execute for Dijkstra
    let result = dijkstra allRecords

    -- Find length of the shortest path to the finish
    print $ minimum
          . map distance
          . filter ((==finishCoord) . fst . state)
          $ result

    print $ "Done."

