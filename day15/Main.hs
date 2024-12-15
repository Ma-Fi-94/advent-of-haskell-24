module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Grid (Coord, enumerate, fromList)
import Utils (thi3, tok)


-- Sugar
type Box     = Coord
type Robot   = Coord
type Wall    = Coord
type Command = Char


-- Does exactly what it says on the tin.
parseInput :: String -> (Robot, Set Wall, Set Box, [Command])
parseInput input = (robot, walls, boxes, commands)
  where
    blocks   = tok [""] . lines $ input
    grid     = fromList (blocks !! 0)
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
             . filter (\ (_, char) -> char == 'O')
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


-- Try to step left
left :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
left (robot@(r, c), walls, boxes)
    | Set.member robot' walls = (robot, walls, boxes)
    | Set.member robot' boxes = case findSpaceLeft (c - 2) of
        Nothing -> (robot, walls, boxes)
        Just i  -> (robot', walls, Set.insert (r, i) (Set.delete robot' boxes))
    | otherwise               = (robot', walls, boxes)
      where
        robot' = (r, c - 1)
        findSpaceLeft :: Int -> Maybe Int
        findSpaceLeft i
            | Set.member (r, i) walls = Nothing
            | Set.member (r, i) boxes = findSpaceLeft (i - 1)
            | otherwise               = Just i


-- Try to step right
right :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
right (robot@(r, c), walls, boxes)
    | Set.member robot' walls = (robot, walls, boxes)
    | Set.member robot' boxes = case findSpaceRight (c + 2) of
        Nothing -> (robot, walls, boxes)
        Just i  -> (robot', walls, Set.insert (r, i) (Set.delete robot' boxes))
    | otherwise               = (robot', walls, boxes)
      where
        robot' = (r, c + 1)
        findSpaceRight :: Int -> Maybe Int
        findSpaceRight i
            | Set.member (r, i) walls = Nothing
            | Set.member (r, i) boxes = findSpaceRight (i + 1)
            | otherwise               = Just i


-- Try to step up
up :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
up (robot@(r, c), walls, boxes)
    | Set.member robot' walls = (robot, walls, boxes)
    | Set.member robot' boxes = case findSpaceUp (r - 2) of
        Nothing -> (robot, walls, boxes)
        Just i  -> (robot', walls, Set.insert (i, c) (Set.delete robot' boxes))
    | otherwise               = (robot', walls, boxes)
      where
        robot' = (r - 1, c)
        findSpaceUp :: Int -> Maybe Int
        findSpaceUp i
            | Set.member (i, c) walls = Nothing
            | Set.member (i, c) boxes = findSpaceUp (i - 1)
            | otherwise               = Just i


-- Try to step down
down :: (Robot, Set Wall, Set Box) -> (Robot, Set Wall, Set Box)
down (robot@(r, c), walls, boxes)
    | Set.member robot' walls = (robot, walls, boxes)
    | Set.member robot' boxes = case findSpaceDown (r + 2) of
        Nothing -> (robot, walls, boxes)
        Just i  -> (robot', walls, Set.insert (i, c) (Set.delete robot' boxes))
    | otherwise               = (robot', walls, boxes)
      where
        robot' = (r + 1, c)
        findSpaceDown :: Int -> Maybe Int
        findSpaceDown i
            | Set.member (i, c) walls = Nothing
            | Set.member (i, c) boxes = findSpaceDown (i + 1)
            | otherwise               = Just i





main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let (robot, walls, boxes, commands) = parseInput fileContents

    print $ sumGPS
          . thi3
          $ run (robot, walls, boxes) commands


    print $ "Done."

