module Main where

import Control.Monad
import Data.List (unfoldr)
import Text.Printf

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

main :: IO ()
main = do
  putHdrRow cols
  forM_ rows (putRow cols)
  where
    putHdrRow cs = do
      putStr "    " >> forM_ cs (printf "%3d") >> putChar '\n'
      putStr "    " >> putStrLn (take (3*length cs) $ repeat '=')
    putRow ys x = printf "%3d|" x >> forM_ ys (putCol x) >> putChar '\n'
    putCol x y = printf "%3d" (x + y)

-- naive
sol rs cs (0, 0) = rs !! 0 + cs !! 0
sol rs cs (0, j) = sol rs cs (0, j-1) + cs !! j
sol rs cs (i, 0) = rs !! i + sol rs cs (i-1, 0)
sol rs cs (i, j) = sol rs cs (i, j-1) + sol rs cs (i-1, j)


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
calcRow r cols = unfoldr phi (r, cols)
  where
    phi (r, []) = Nothing
    phi (r, c:cs) = Just (r+c, (r+c, cs))

-- calcMatrix [] cs = []
-- calcMatrix (r:rs) cs = let ps = calcRow r cs in ps:calcMatrix rs ps

-- cm ([],cs) = []
-- cm (r:rs, cs) = let ps = calcRow r cs in ps:cm (rs, ps)

calcMatrix :: ([Int],[Int]) -> [[Int]]
calcMatrix = unfoldr psi
  where
    psi ([],   cs) = Nothing
    psi (r:rs, cs) = Just (ps, (rs, ps))
      where
        ps = calcRow r cs

draw :: (([Int], [Int]) -> [[Int]]) -> [Int] -> [Int] -> IO ()
draw calc rs cs = do
  putHdr
  forM_ (zip rs anss) putRow
  where
    anss = calc (rs, cs)
    digits = length . show . maximum
    (dr, dr', dr'') = (digits rs, dr+1, dr'+1)
    (ds, ds') = (digits (map maximum anss), ds+1)
    pf = printf ("%" ++ show ds' ++ "d")
    indent = putStr (replicate dr'' ' ')
    
    putHdr = do
      indent >> forM_ cs pf >> putChar '\n'
      indent >> putStr (replicate (length cs * ds') '=') >> putChar '\n'
    putRow (x, ys) = do
      printf ("%" ++ show dr' ++ "d|") x
      forM_ ys pf >> putChar '\n'
