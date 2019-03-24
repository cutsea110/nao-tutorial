module Collatz where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List

collatz :: Integral a => a -> [a]
collatz n = go n []
  where
    go n ret | n <= 1 = 1:ret
             | even n = go (n `div` 2) (n:ret)
             | otherwise = go (3 * n + 1) (n:ret)

collatzLen mxN = maximumBy (compare `on` snd) $ map ((id &&& length) . collatz) [1..mxN]
