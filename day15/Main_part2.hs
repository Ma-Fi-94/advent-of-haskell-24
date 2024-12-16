module Main where

import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Grid (Coord, enumerate, fromList)
import Utils (groupn, thi3, tok)

-- Sugar
type Box     = Coord
type Robot   = Coord
type Wall    = Coord
type Command = Char


-- Preprocessing of an input grid's line for part 2. This uses a
-- very naive character replacement approach, but it should do
-- the trick regardless.
widenLine :: String -> String
widenLine ""       = ""
widenLine ('#':cs) = "##" ++ widenLine cs
widenLine ('O':cs) = "[]" ++ widenLine cs
widenLine ('.':cs) = ".." ++ widenLine cs
widenLine ('@':cs) = "@." ++ widenLine cs


-- Does exactly what it says on the tin.
parseInput :: String -> (Robot, Set Wall, Set Box, [Command])
parseInput input = (robot, walls, boxes, commands)
  where
    blocks   = tok [""] . lines $ input
    grid     = fromList $ map widenLine (blocks !! 0)
    commands = concat (blocks !! 1)

    robot    = fst
             . head
             . filter (\ (_, char) -> char == '@')
             $ enumerate grid
    walls    = Set.fromList
             . map fst
             . filter (\ (_, char) -> char == '#')
             $ enumerate grid
    boxes    = Set.fromList
             . map fst
             . filter (\ (_, char) -> char == '[')
             $ enumerate grid


-- Recursively executes all commands in the list. We could also use a fold here,
-- but this works nicely too.
run :: (Robot, Set Wall, Set Box) -> [Command] -> (Robot, Set Wall, Set Box)
run state []       = state
run state ('<':cs) = run (left state)  cs
run state ('>':cs) = run (right state) cs
run state ('^':cs) = run (up state)    cs
run state ('v':cs) = run (down state)  cs


-- Calculate final score
sumGPS :: Set Box -> Int
sumGPS = sum
       . map (\ (r, c) -> 100 * r + c)
       . Set.elems


-- Try to move left
left :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
left (robot@(r, c), walls, boxes)
    | Set.member robot' walls     = (robot, walls, boxes)
    | Set.member (r, c - 2) boxes = case firstFreeLeft (c - 2) of
        Nothing -> (robot, walls, boxes)
        Just i  -> (robot', walls, boxes')
          where
            boxes' = (`Set.union`      (Set.fromList [(r, x) | x <- [i, i+2 .. c-3]]))
                   . (`Set.difference` (Set.fromList [(r, x) | x <- [i+1, i+3 .. c-2]]))
                   $ boxes
    | otherwise                   = (robot', walls, boxes)
      where
        robot' = (r, c - 1)
        firstFreeLeft :: Int -> Maybe Int
        firstFreeLeft i
            | Set.member (r, i + 1) walls = Nothing
            | Set.member (r, i) boxes     = firstFreeLeft (i - 2)
            | otherwise                   = Just (i + 1)
        

-- Try to move right
right :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
right (robot@(r, c), walls, boxes)
    | Set.member robot' walls = (robot, walls, boxes)
    | Set.member robot' boxes = case firstFreeRight (c + 3) of
        Nothing -> (robot, walls, boxes)
        Just i  -> (robot', walls, boxes')
          where
            boxes' = (`Set.union`      (Set.fromList [(r, x) | x <- [c+2, c+4 .. i-1]]))
                   . (`Set.difference` (Set.fromList [(r, x) | x <- [c+1, c+3 .. i-2]]))
                   $ boxes
    | otherwise               = (robot', walls, boxes)
      where
        robot' = (r, c + 1)
        firstFreeRight :: Int -> Maybe Int
        firstFreeRight i
            | Set.member (r, i) walls = Nothing
            | Set.member (r, i) boxes = firstFreeRight (i + 2)
            | otherwise               = Just i


-- Try to move up
up :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
up (robot@(r, c), walls, boxes)
    | Set.member robot' walls         = (robot, walls, boxes)
    | Set.member robot' boxes         = case findChunkUp (walls, boxes) robot' of
        Nothing      -> (robot, walls, boxes)
        (Just chunk) -> (robot', walls, boxes')
          where
            boxes' = (boxes `Set.difference` (Set.fromList chunk)) `Set.union` (Set.fromList (map (\ (m, n) -> (m - 1, n)) chunk))
    | Set.member (r - 1, c - 1) boxes = case findChunkUp (walls, boxes) (r - 1, c - 1) of
        Nothing      -> (robot, walls, boxes)
        (Just chunk) -> (robot', walls, boxes')
          where
            boxes' = (boxes `Set.difference` (Set.fromList chunk)) `Set.union` (Set.fromList (map (\ (m, n) -> (m - 1, n)) chunk))
    | otherwise                       = (robot', walls, boxes)
      where
        robot' = (r - 1, c)


-- Given one box, identify all boxes that need to be moved up with it.
-- If the chunk cannot be moved, return Nothing.
findChunkUp :: (Set Wall, Set Box) -> Box -> Maybe [Box]
findChunkUp (walls, boxes) box
    | any (==Nothing) (recursiveSearch box) = Nothing
    | otherwise                             = Just $ (box : map fromJust (recursiveSearch box))
  where
    -- Given a guaranteed Box, look for all the other boxes that have to move upwards with it
    recursiveSearch :: Box -> [(Maybe Box)]
    recursiveSearch (r, c)
        | (r - 1, c) `Set.member` walls     = [Nothing]
        | (r - 1, c + 1) `Set.member` walls = [Nothing]
        | (r - 1, c) `Set.member` boxes     = (Just (r - 1, c)) : recursiveSearch (r - 1, c)
        | (r - 1, c - 1) `Set.member` boxes && (r - 1, c + 1) `Set.member` boxes = (Just (r - 1, c - 1))
                                                                                 : (Just (r - 1, c + 1))
                                                                                 : (recursiveSearch (r - 1, c - 1)
                                                                                   ++ (recursiveSearch (r - 1, c + 1)))
        | (r - 1, c - 1) `Set.member` boxes = (Just (r - 1, c - 1)) : recursiveSearch (r - 1, c - 1)
        | (r - 1, c + 1) `Set.member` boxes = (Just (r - 1, c + 1)) : recursiveSearch (r - 1, c + 1)
        | otherwise = [Just (r, c)]




-- Try to move down
down :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
down (robot@(r, c), walls, boxes)
    | Set.member robot' walls         = (robot, walls, boxes)
    | Set.member robot' boxes         = case findChunkDown (walls, boxes) robot' of
        Nothing      -> (robot, walls, boxes)
        (Just chunk) -> (robot', walls, boxes')
          where
            boxes' = (boxes `Set.difference` (Set.fromList chunk)) `Set.union` (Set.fromList (map (\ (m, n) -> (m + 1, n)) chunk))
    | Set.member (r + 1, c - 1) boxes = case findChunkDown (walls, boxes) (r + 1, c - 1) of
        Nothing      -> (robot, walls, boxes)
        (Just chunk) -> (robot', walls, boxes')
          where
            boxes' = (boxes `Set.difference` (Set.fromList chunk)) `Set.union` (Set.fromList (map (\ (m, n) -> (m + 1, n)) chunk))
    | otherwise                       = (robot', walls, boxes)
      where
        robot' = (r + 1, c)


-- Given one box, identify all boxes that need to be moved up with it.
-- If the chunk cannot be moved, return Nothing.
findChunkDown :: (Set Wall, Set Box) -> Box -> Maybe [Box]
findChunkDown (walls, boxes) box
    | any (==Nothing) (recursiveSearch box) = Nothing
    | otherwise                             = Just $ (box : map fromJust (recursiveSearch box))
  where
    -- Given a guaranteed Box, look for all the other boxes that have to move upwards with it
    recursiveSearch :: Box -> [(Maybe Box)]
    recursiveSearch (r, c)
        | (r + 1, c) `Set.member` walls     = [Nothing]
        | (r + 1, c + 1) `Set.member` walls = [Nothing]
        | (r + 1, c) `Set.member` boxes     = (Just (r + 1, c)) : recursiveSearch (r + 1, c)
        | (r + 1, c - 1) `Set.member` boxes && (r + 1, c + 1) `Set.member` boxes = (Just (r + 1, c - 1))
                                                                                 : (Just (r + 1, c + 1))
                                                                                 : (recursiveSearch (r + 1, c - 1)
                                                                                   ++ (recursiveSearch (r + 1, c + 1)))
        | (r + 1, c - 1) `Set.member` boxes = (Just (r + 1, c - 1)) : recursiveSearch (r + 1, c - 1)
        | (r + 1, c + 1) `Set.member` boxes = (Just (r + 1, c + 1)) : recursiveSearch (r + 1, c + 1)
        | otherwise = [Just (r, c)]


-- For debugging:
repr :: (Robot, Set Wall, Set Box) -> [String]
repr (robot, walls, boxes) = groupn 20
                           . map transform
                           $ [(i, j) | i <- [0..9], j <- [0..19]]
  where
    transform coord@(r, c)
        | coord == robot                = '@'
        | coord `Set.member` walls      = '#'
        | coord `Set.member` boxes      = '['
        | (r, c - 1) `Set.member` boxes = ']'
        | otherwise                     = '.'


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let (robot, walls, boxes, commands) = parseInput fileContents
    let (_, _, boxesFinal) = run (robot, walls, boxes) commands
    print $ sumGPS boxesFinal
 
    print $ "Done."

