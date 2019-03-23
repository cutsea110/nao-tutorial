module Main where

import Control.Monad
import Data.List (unfoldr)
import Text.Printf

import DrawMatrix (draw)

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

main :: IO ()
main = draw (Just 3) f rows cols
  where
    f (rs, cs) = [[r + c | c <- cs] | r <- rs]

main2 :: IO ()
main2 = draw (Just 6) calcMatrix rows cols

main3 :: IO ()
main3 = draw (Just 10) sol' rows cols

main4 :: IO ()
main4 = draw (Just 3) sol' xs xs
  where
    xs = replicate 20 0

-- naive
sol rs cs (0, 0) = rs !! 0 + cs !! 0
sol rs cs (0, j) = sol rs cs (0, j-1) + cs !! j
sol rs cs (i, 0) = rs !! i + sol rs cs (i-1, 0)
sol rs cs (i, j) = sol rs cs (i, j-1) + sol rs cs (i-1, j)

sol' :: ([Int], [Int]) -> [[Int]]
sol' (rs, cs) = [[sol rs cs (i, j) | j <- [0..c']] | i <- [0..r']]
  where
    (r', c') = (length rs - 1, length cs - 1)

mkMatrix :: [Int] -> [Int] -> IO ()
mkMatrix rs cs = do
  forM_ [0..r'] $ \r -> do
    forM_ [0..c'] $ \c -> do
      printf "%13d" $ sol' (r, c)
    putChar '\n'
  where
    r' = length rs -1
    c' = length cs -1
    sol' = sol rs cs

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

-- sol' (r, []) = (r, [])
-- sol' (r, c:cs) = (r+c, cs)
calcRow :: Num a => a -> [a] -> [a]
calcRow r cols = unfoldr phi (r, cols)
  where
    phi (r, []) = Nothing
    phi (r, c:cs) = Just (r+c, (r+c, cs))

calcMatrix :: ([Int],[Int]) -> [[Int]]
calcMatrix = unfoldr psi
  where
    psi ([],   cs) = Nothing
    psi (r:rs, cs) = Just (ps, (rs, ps))
      where
        ps = calcRow r cs
