-- This code is from https://raw.githubusercontent.com/extremMiSt/adventofcode24/05e0ac9c030fdb7c9d2f7cff73804febfddeb895/day21/app/Main.hs
-- We spent extraordinary amounts of hours on thinking about this one, but his solution turned out to be more performant and he found
-- the last bug today that kept us from getting the right answer.

module Main where
import Data.List (nub, genericLength)
import Data.Map (fromList, Map, (!))

type Point = (Int,Int)
type Pad = [[String]]
type Robot = (Point, Pad)

numCoord :: Char -> Point
numCoord 'A' = (2,3)
numCoord '0' = (1,3)
numCoord '1' = (0,2)
numCoord '2' = (1,2)
numCoord '3' = (2,2)
numCoord '4' = (0,1)
numCoord '5' = (1,1)
numCoord '6' = (2,1)
numCoord '7' = (0,0)
numCoord '8' = (1,0)
numCoord '9' = (2,0)
numCoord _ = error "how?"

mapping :: Char -> Char -> String
mapping 'A' '<' = "v<<A"
mapping 'A' '>' = "vA"
mapping 'A' '^' = "<A"
mapping 'A' 'v' = "<vA"

mapping '<' 'A' = ">>^A"
mapping '<' '>' = error "why?"
mapping '<' '^' = ">^A"
mapping '<' 'v' = ">A"

mapping '>' 'A' = "^A"
mapping '>' '<' = error "why?"
mapping '>' '^' = "<^A"
mapping '>' 'v' = "<A"

mapping '^' 'A' = ">A"
mapping '^' '>' = "v>A"
mapping '^' '<' = "v<A"
mapping '^' 'v' = error "why?"

mapping 'v' 'A' = "^>A" --">^A" --this number cost me a few hours ...
mapping 'v' '<' = "<A"
mapping 'v' '>' = ">A"
mapping 'v' '^' = error "why?"

mapping a b | a==b = "A"
mapping a b = error ("dang " ++  [a] ++ " " ++ [b] ++"!")

mapper :: Char -> String -> String
mapper _ [] = []
mapper c (x:xs) = mapping c x ++ mapper x xs

numPaths :: Char -> Char -> [String]
numPaths start goal= nub $ xFirst ++ yFirst
    where
        (sx, sy) = numCoord start
        (gx, gy) = numCoord goal
        xDiff = sx - gx
        yDiff = sy - gy
        xDir = replicate (abs xDiff) (if xDiff >= 0 then '<' else '>')
        yDir = replicate (abs yDiff) (if yDiff >= 0 then '^' else 'v')
        xFirst = [xDir ++ yDir | sy /= 3 || gx /= 0]
        yFirst = [yDir ++ xDir | sx /= 0 || gy /= 3]

paths :: [[Char]] -> Char -> [Char] -> [[Char]]
paths prefix _ [] = prefix
paths prefix cur (a:as) = paths (concatMap (\x -> [x++p++"A" | p<- numPaths cur a]) prefix) a as

score :: String -> Integer
score str = read (take 3 str) * minimum (map (genericLength . mapper 'A' . mapper 'A') (paths [""] 'A' str))

part1 :: String -> Integer
part1 f = sum $ map score $ lines f

mapping' :: Map (Char,Char) Integer
mapping' = fromList [
    (('A','<'), 4),
    (('A','>'), 2),
    (('A','^'), 2),
    (('A','v'), 3),
    (('A','A'), 1),
    (('<','A'), 4),
    (('<','>'), error "why?"),
    (('<','^'), 3),
    (('<','v'), 2),
    (('<','<'), 1),
    (('>','A'), 2),
    (('>','<'), error "why?"),
    (('>','^'), 3),
    (('>','v'), 2),
    (('>','>'), 1),
    (('^','A'), 2),
    (('^','>'), 3),
    (('^','<'), 3),
    (('^','v'), error "why?"),
    (('^','^'), 1),
    (('v','A'), 3),
    (('v','<'), 2),
    (('v','>'), 2),
    (('v','^'), error "why?"),
    (('v','v'), 1)]

addLayer :: Map (Char,Char) Integer -> Map (Char,Char) Integer
addLayer old = fromList [
        (('A','<'), next 'A' '<'),
        (('A','>'), next 'A' '>'),
        (('A','^'), next 'A' '^'),
        (('A','v'), next 'A' 'v'),
        (('A','A'), next 'A' 'A'),
        (('<','A'), next '<' 'A'),
        (('<','>'), error "why?"),
        (('<','^'), next '<' '^'),
        (('<','v'), next '<' 'v'),
        (('<','<'), next '<' '<'),
        (('>','A'), next '>' 'A'),
        (('>','<'), error "why?"),
        (('>','^'), next '>' '^'),
        (('>','v'), next '>' 'v'),
        (('>','>'), next '>' '>'),
        (('^','A'), next '^' 'A'),
        (('^','>'), next '^' '>'),
        (('^','<'), next '^' '<'),
        (('^','v'), error "why?"),
        (('^','^'), next '^' '^'),
        (('v','A'), next 'v' 'A'),
        (('v','<'), next 'v' '<'),
        (('v','>'), next 'v' '>'),
        (('v','^'), error "why?"),
        (('v','v'), next 'v' 'v')]
    where 
        next prev cur = sum $ map (old!) (pair $ 'A':mapping prev cur)
        pair [] = []
        pair [_] = []
        pair (x:xx:xs) = (x,xx) : pair (xx:xs) 

layerZ :: Map (Char, Char) Integer
layerZ = rep (z-1) addLayer mapping'

rep :: Int -> (a->a) -> (a->a)
rep n f | n == 0 = id
        | otherwise = f . rep (n-1) f

mapperMap :: Map (Char,Char) Integer -> Char -> String -> Integer
mapperMap _ _ [] = 0
mapperMap maps c (x:xs) =  maps!(c,x) + mapperMap maps x xs

scoreZ :: String -> Integer
scoreZ str = read (take 3 str) * minimum (map (mapperMap layerZ 'A') (paths [""] 'A' str))

part2 :: String -> Integer
part2 f = sum $ map scoreZ $ lines f

z :: Int
z = 25

main :: IO ()
main = do
    f <- readFile "./input.txt"
    print $ part1 f
    print $ part2 f