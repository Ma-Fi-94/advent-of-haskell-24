--module Vector (vec, elems, zeroes, zeroesLike, vadd, vsub, smul, vzipWith) where
module Vector where

data Vec a = V Int [a] deriving (Show, Eq)


vec :: [a] -> Vec a
vec xs = V (length xs) xs


elems :: Vec a -> [a]
elems (V _ xs) = xs


zeroes :: Int -> Vec Int
zeroes n = V n $ replicate n 0


zeroesLike :: Vec a -> Vec Int
zeroesLike (V n _) = zeroes n


vadd :: Num a => Vec a -> Vec a -> Vec a
vadd (V i xs) (V j ys)
    | i /= j    = error "Vector.add: Incompatible vector lengths."
    | otherwise = V i $ zipWith (+) xs ys


vsub :: Num a => Vec a -> Vec a -> Vec a
vsub v1 v2 = vadd v1 $ smul (-1) v2


smul :: Num a => a -> Vec a -> Vec a
smul c (V i xs) = V i $ map (*c) xs


hadprod :: Num a => Vec a -> Vec a -> Vec a
hadprod (V i xs) (V j ys)
    | i /= j    = error "Vector.hadprod: Incompatible vector lengths."
    | otherwise = V i $ zipWith (*) xs ys


dot :: Num a => Vec a -> Vec a -> a
dot (V i xs) (V j ys)
    | i /= j    = error "Vector.hadprod: Incompatible vector lengths."
    | otherwise = sum $ zipWith (*) xs ys


vmap :: (a -> b) -> Vec a -> Vec b
vmap f (V i xs) = V i (map f xs)


vzipWith :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
vzipWith f (V i xs) (V j ys)
    | i /= j    = error "Vector.vzipWith: Incompatible lengths."
    | otherwise = V i (zipWith f xs ys)


-- implement Ord manually for this ?

vandZipWith_ :: (a -> a -> Bool) -> Vec a -> Vec a -> Bool
vandZipWith_ f v1 v2 = and $ elems (vzipWith f v1 v2)


lt :: Ord a => Vec a -> Vec a -> Bool
lt = vandZipWith_ (<)


le :: Ord a => Vec a -> Vec a -> Bool
le = vandZipWith_ (<=)


gt :: Ord a => Vec a -> Vec a -> Bool
gt = vandZipWith_ (>)


ge :: Ord a => Vec a -> Vec a -> Bool
ge = vandZipWith_ (>=)