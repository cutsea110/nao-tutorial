module Cons where

cons x xs = \f -> f x xs
car p = p (\x xs -> x)
cdr p = p (\x xs -> xs)
