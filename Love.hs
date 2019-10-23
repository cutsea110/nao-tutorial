module Main where

import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List
import qualified Data.Map as Map

main = do
  names <- getLine
  let ns = name2num names
      len = length ns
      ans = scanl (\xs _ -> zipWith (+%) xs (tail xs)) ns [1..len-2]
  forM_ (zip [0..] ans) $ \(i, ln) -> do
    putStr $ replicate i ' '
    putStrLn $ toLine ln

toLine = concat . intersperse " " . map show

name2num :: [Char] -> [Int]
name2num = map conv . filter (`elem` boin) . map toLower

boin :: [Char]
boin = "aiueo"

conv :: Char -> Int
conv 'a' = 1
conv 'i' = 2
conv 'u' = 3
conv 'e' = 4
conv 'o' = 5

(+%) :: Int -> Int -> Int
x +% y = (x + y) `mod` 10
