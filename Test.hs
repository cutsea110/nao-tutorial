module Main where

import Control.Monad
import Text.Printf

rows,cols :: [Int]
rows = [4,2,5,6,7,1,3,9,3,2]
cols = [8,2,4,6,1,8,9,3,1,7]

main :: IO ()
main = do
  putHdrRow cols
  forM_ rows $ \r -> do
    putRow r cols
  where
    putHdrRow cs = do
      putStr "    "
      forM_ cs (printf "%3d")
      putStrLn ""
      putStr "    "
      putStrLn $ take (3*length cs) $ repeat '='
    putRow x ys = do
      printf "%3d|" x
      forM_ ys $ \y -> do
        putCol x y
      putStrLn ""
    putCol x y = printf "%3d" (x + y)

sol rs cs (0, 0) = rs !! 0 + cs !! 0
sol rs cs (0, j) = sol rs cs (0, j-1) + cs !! j
sol rs cs (i, 0) = rs !! i + sol rs cs (i-1, 0)
sol rs cs (i, j) = sol rs cs (i, j-1) + sol rs cs (i-1, j)
