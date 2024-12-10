module Main where

-- This is almost entirely based on the excellent solution by my buddy
-- https://github.com/extremMiSt/adventofcode24/blob/main/day09/app/Main.hs.
-- I merely slightly refactored it, and provided some comments to ease
-- understanding. If I had not visited the Christmas market today, I (maybe?)
-- would have been able to solve this all by myself ;D.

import Utils (readInteger)
import Data.List (delete, insert, sort)

type Offset  = Integer
type Length  = Integer
type Content = Integer

type FreeBlock = (Offset, Length)
type FullBlock = (Offset, Length, Content)


-- Calculate the checksum of a fully expanded memory dump.
-- Empty cells (containing -1) are skipped.
checksum :: [Integer] -> Integer
checksum = go 0
  where
    go _ []      = 0
    go i (-1:xs) = go (i + 1) xs
    go i (x:xs)  = i * x + go (i + 1) xs


-- Given the final list of occupied blocks, expand it into a memory dump.
-- Free spaces are filled with (-1), as it is convention.
-- We only need this function for calculating the checksum at the end.
expand :: [FullBlock] -> [Integer]
expand = go 0
    where
        go _ []                 = []
        go n ((_, 0, _):blocks) = go n blocks
        go n block@((pos, len, cont):blocks)
            | n < pos   = -1   : go (n + 1) block
            | otherwise = cont : go (n + 1) ((pos, len - 1, cont):blocks) 


-- Given a list of lengths of blocks, extract the free blocks.
-- Those are blocks 1, 3, etc, so the 0th block is occupied and
-- needs to be skipped. Returns a list of tuples (offset, length)
extractFreeBlocks :: [Length] -> [FreeBlock]
extractFreeBlocks = go True 0
    where
        go _     _   []         = []
        go True  pos (len:lens) = go False (pos + len) lens
        go False pos (len:lens) = (pos, len) : go True (pos + len) lens 


-- The same as before, but for full blocks, now returning tuples
-- of (offset, length, content)
extractFullBlocks :: [Length] -> [FullBlock]
extractFullBlocks = go True 0 0
    where
        go _     _    _   []         = []
        go True  pos cont (len:lens) = (pos, len, cont)
                                     : go False (pos + len) (cont + 1) lens
        go False pos cont (len:lens) = go True  (pos + len)  cont      lens


-- Given the list of free blocks and one full block, find out where the
-- full block moves to. It may also stay where it is if no free block
-- of sufficient size on the left hand side of the full block is available.
moveLeft :: [FreeBlock] -> FullBlock -> FreeBlock
moveLeft freeBlocks (pos, len, _) = if null candidates
                                    then (pos, len)
                                    else head candidates
    where
        candidates = filter (\(p, l) -> len <= l && p < pos) freeBlocks


-- Using the previous function, move all full blocks in the given list
-- to their left-most place.
moveAll :: [FullBlock] -> [FreeBlock] -> [FullBlock]
moveAll [] _ = []
moveAll (block@(_, len, c):blocks) freeBlocks
    | diff == 0 = (p, len, c) : moveAll blocks (delete goal freeBlocks)
    | otherwise = (p, len, c) : moveAll blocks (insert (p + len, diff) (delete goal freeBlocks))
      where 
        goal@(p, l) = moveLeft freeBlocks block
        diff        = l - len


-- The wrapper that starts it all. We generate two lists, and reverse the list
-- of full blocks, so we can access the ends in O(1).
defragment :: [Length] -> [FullBlock]
defragment l = sort $ moveAll fullBlocks freeBlocks
    where 
        fullBlocks = reverse $ extractFullBlocks l
        freeBlocks = extractFreeBlocks l


main :: IO ()
main = do
    fileContents <- readFile "input.txt"
    let input     = map (readInteger . (:[])) fileContents
    
    print $ checksum . expand . defragment $ input







