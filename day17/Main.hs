module Main where

import Data.Bits (xor)
import Utils ((!?))

-- Sugar
type IP           = Int
type Register     = Integer
type State        = (Register, Register, Register, IP)
type Instructions = [Integer]


-- Run the entire list of instructions given a starting value for a.
-- Output is returned as list of ints.
run :: Instructions -> Integer -> [Integer]
run instructions a = go (a, 0, 0, 0)
  where
    go (a, b, c, ip) = case opcode of
        Just 0  -> go (a `div` (2 ^ comboOperand), b, c, ip')
        Just 1  -> go (a, b `xor` operand, c, ip')
        Just 2  -> go (a, comboOperand `mod` 8, c, ip')
        Just 3  -> if   a == 0
                   then go (a, b, c, ip')
                   else go (a, b, c, fromInteger operand)
        Just 4  -> go (a, b `xor` c, c, ip')
        Just 5  -> comboOperand `mod` 8 : go (a, b, c, ip')
        Just 6  -> go (a, a `div` (2 ^ comboOperand), c, ip')
        Just 7  -> go (a, b, a `div` (2 ^ comboOperand), ip')
        Nothing -> []
      where
        opcode  = instructions !? ip
        operand = instructions !! (ip + 1)
        ip'     = ip + 2
        comboOperand
            | operand `elem` [0..3] = operand
            | operand == 4          = a
            | operand == 5          = b
            | operand == 6          = c


main :: IO ()
main = do
    -- Part 1
    -- Not doing any automatic parsing today, since the input is tiny.
    let instructions = [2,4,1,1,7,5,1,5,4,1,5,5,0,3,3,0]
    print $ run instructions 17323786


    -- Part 2: Key insight is to individually assess blocks of three bits, i.e.
    -- numbers from 0..7. I manually tried a BFS multiple times, but must have
    -- made some errors along the way every time. However, this gave me the bounds
    -- 164278764924544 < soln < 164281784823485, which we can easily bruteforce.
    -- This yielded 164278764924605, which thankfully was right :D.

    print $ fst
          . head
          . filter ((==instructions) . snd)
          . map (\ i -> (i, run instructions i))
          $ [164278764924544..164281784823485]



    print $ "Done."

