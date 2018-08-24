module Tst where

getParam str = foldr f [[]] str
    where
    f x rest@(r:rs)
       | x == ' '  = [] : rest
       | otherwise = (x : r) : rs