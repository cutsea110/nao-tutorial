module DP
  ( DP
  , dp
  , evalDP, evalDPAll
  ) where

import qualified Data.Map
import Control.Monad.State

type Memo a b = Data.Map.Map a b
type DP a b = a -> State (Memo a b) b

emptyMemo :: Memo a b
emptyMemo = Data.Map.empty
lookupMemo :: Ord a => a -> Memo a b -> Maybe b
lookupMemo = Data.Map.lookup
insertMemo :: Ord a => a -> b -> Memo a b -> Memo a b
insertMemo = Data.Map.insert

dp :: Ord a => DP a b -> DP a b
dp f x = do
  memo <- gets (lookupMemo x)
  case memo of
       Just y  -> return y
       Nothing -> do
         y <- f x
         modify (insertMemo x y)
         return y

evalDP :: DP a b -> a -> b
evalDP f x = evalState (f x) emptyMemo

evalDPAll :: DP a b -> [a] -> [b]
evalDPAll f xs = evalState (sequence (map f xs)) emptyMemo
