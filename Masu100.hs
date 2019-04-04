module Main where

import Control.Monad
import Data.List (unfoldr)
import Text.Printf

import DrawMatrix (draw, draw')

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

main :: IO ()
main = draw' 3 f cols rows
  where
    f (cs, rs) = [[r + c | c <- cs] | r <- rs]

main2 :: IO ()
main2 = draw' 6 tabulation cols rows

main3 :: IO ()
main3 = draw' 6 naive cols rows

main4 :: IO ()
main4 = draw' 3 naive xs xs
  where
    xs = replicate 20 0

-- naive
naive :: ([Int], [Int]) -> [[Int]]
naive (cs, rs) = [[nv (i, j) | j <- [0..c']] | i <- [0..r']]
  where
    (c', r') = (length cs - 1, length rs - 1)
    nv (0, 0) = rs !! 0 + cs !! 0
    nv (0, j) = nv (0, j-1) + cs !! j
    nv (i, 0) = rs !! i + nv (i-1, 0)
    nv (i, j) = nv (i, j-1) + nv (i-1, j)

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-
    0   1   2   3   4   5   6   7   8   9  10
----------------------------------------------
fib 1   1   2   3   5   8  13  21  34  55  89
      \   \   \   \   \   \   \   \   \   \
    0   1   1   2   3   5   8  13  21  34  55
-}
fib' 0 = (0, 1)
fib' n = let (x, y) = fib' (n-1) in (y, x+y)

foldn (c, f) = u
  where
    u 0 = c
    u n = f (u (n-1))

pair (f, g) x = (f x, g x)
fib'' = snd . foldn ((0, 1), pair (snd, (+) <$> fst <*> snd))

{-
      8  2  4  6  1  8  9  3  1  7
    ==============================
  4| 12 14 18 24 25 33 42 45 46 53
-}

winder :: ((a, b) -> c) -> (b, [a]) -> Maybe (c, (c, [a]))
winder f (y, xxs) = case xxs of
  []     -> Nothing
  (x:xs) -> Just (y', (y', xs)) where y' = f (x, y)

calcRow :: (Int, [Int]) -> [Int]
calcRow = unfoldr (winder plus) where plus = uncurry (+)

tabulation :: ([Int],[Int]) -> [[Int]]
tabulation = unfoldr (winder calcRow)
