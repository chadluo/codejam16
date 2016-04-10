module CountingSheep where

-- import           Data.List
import           Data.Set

f :: Int -> [Int]
f x = iterate (+ x) x

test :: Int -> Int -> Bool
test x y = (fromList ['0'..'9'] `isSubsetOf`) $ fromList $ concatMap show $ take y $ f x

s :: Int -> String
s x = s' x [1..1000]

s' :: Int -> [Int] -> String
s' _ [] = "INSOMNIA"
s' x (y:ys) = if test x y then show (x * y) else s' x ys

main :: IO ()
main = interact (unlines . deal . mapRead . lines)

mapRead :: [String] -> [Int]
mapRead = Prelude.map read

deal :: [Int] -> [String]
deal (x:xs) = deal' [1..x] xs

deal' :: [Int] -> [Int] -> [String]
deal' _ [] = []
deal' (x:xs) (y:ys) = ("Case #" ++ show x ++ ": " ++ s y) : deal' xs ys
