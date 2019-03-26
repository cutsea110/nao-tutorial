module Collatz where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List
import DP

-- naive
collatz :: Integral a => a -> [a]
collatz n = go n []
  where
    go n ret | n <= 1 = 1:ret
             | even n = go (n `div` 2) (n:ret)
             | otherwise = go (3 * n + 1) (n:ret)

collatzLen mxN = maximumBy (compare `on` snd) $ map ((id &&& length) . collatz) [1..mxN]

-- use DP
collatz' :: DP Integer [Integer]
collatz' = dp (go [])
  where
    go ret = \n ->
      if n <= 1
      then return (1:ret)
      else if even n
           then go (n:ret) (n `div` 2)
           else go (n:ret) (3 * n + 1)

solve' mxN = maximumBy (compare `on` (length.fst))  . flip zip [1..] . evalDPAll collatz' $ [1..mxN]

-- ref.) http://d.hatena.ne.jp/toslunar/touch/20100408/1270719176
solve nMax = snd . maximum . flip zip [1..] . evalDPAll collatzLength $ [1..nMax]

collatzLength = dp $ \n ->
  if n == 1
  then return 0
  else do
    l <- collatzLength (collatzIter n)
    return $ l + 1

collatzIter n = if even n then (n `div` 2) else (3 * n + 1)

