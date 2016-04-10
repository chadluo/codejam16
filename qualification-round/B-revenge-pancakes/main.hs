module RevengePancakes where

import           Data.List

main :: IO ()
main = interact (unlines . deal . lines)

deal :: [String] -> [String]
deal (x:xs) = deal' [1..(read::String->Int) x] xs

deal' :: [Int] -> [String] -> [String]
deal' _ [] = []
deal' (x:xs) (y:ys) = ("Case #" ++ show x ++ ": "++ (show . flipCount) y) : deal' xs ys

flipCount :: String -> Int
flipCount s = length s' - if '+' `elem` last s' then 1 else 0 where
  s' = group s
