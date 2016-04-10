{-# LANGUAGE ParallelListComp #-}

module Main where

import           Data.List

main :: IO ()
main = interact (unlines . deal . lines)

deal :: [String] -> [String]
deal xs = deal' [1..x'] (tail xs) where
  x' = ((read :: String -> Int) . head) xs

deal' :: [Int] -> [String] -> [String]
deal' _ [] = []
deal' [] _ = []
deal' (x:xs) (y:ys) = ("Case #" ++ show x ++ ": " ++ result) : deal' xs ys where
  params = map (read :: String -> Int) $ words y
  result = case test params of
    Nothing -> "IMPOSSIBLE"
    Just s -> (unwords . map (show . succ)) s

test :: [Int] -> Maybe [Int]
test [k,c,s] = find f (combinations s [0..k^c-1]) where
  ps = p k c
  f ss = all (\e -> any (e !!) ss) ps
test _ = Nothing

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations n xs = [y:ys | y:xs' <- tails xs, ys <- combinations (n-1) xs']

p :: Int -> Int -> [[Bool]]
p k c = init $ map (iter c) (origs k)

iter :: Int -> [Bool] -> [Bool]
iter x str = foldl (.) id (replicate (x - 1) (step str)) str

step :: [Bool] -> [Bool] -> [Bool]
step _ [] = []
step orig (x:xs) | x = replicate (length orig) True ++ step orig xs
                 | otherwise = orig ++ step orig xs

origs :: Int -> [[Bool]]
origs 0 = [[]]
origs x = [m : o | m <- [True, False] | o <- origs (x - 1)]
