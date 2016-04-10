module Jamcoin where

import           Data.Char
import           Data.List
import           Numeric

main :: IO ()
main = interact (unlines . deal . lines)

deal :: [String] -> [String]
deal xs = ("Case #" ++ head xs ++ ":") : (deal' . tail) xs

deal' :: [String] -> [String]
deal' = map (\x -> let p = (map (read :: String -> Int) . words) x
                       p1 = p !! 0
                       p2 = p !! 1
                    in jamcoin p1 p2)

jamcoin :: Int -> Int -> String
jamcoin n = jc ('1':replicate (n-2) '0'++['1'])

jc :: String -> Int -> String
jc _ 0 = []
jc n j = case firstDivisors n of
  Nothing -> jc (bininc n) j
  Just str -> n ++ " " ++ str ++ "\n" ++ jc (bininc n) (j - 1)

bininc :: String -> String
bininc x = let x' = bininc' x in if last x' == '0' then bininc' x' else x'

bininc' :: String -> String
bininc' = reverse . concat . inc . group . reverse

inc :: [String] -> [String]
inc [] = []
inc (x:xs) | '1' `elem` x = map pred x : inc xs
           | '0' `elem` x = ('1':tail x) : xs
           | otherwise = []

fromBase :: Int -> String -> Int
fromBase base = fst . head . readInt base ((< base) . digitToInt) digitToInt

toBase :: Int -> Int -> String
toBase base num = showIntAtBase base intToDigit num ""

fromBaseToBase :: Int -> Int -> String -> String
fromBaseToBase from to = toBase to . fromBase from

bases :: String -> [Int]
bases str = map (\x -> (read :: String -> Int) $ fromBaseToBase x 10 str) [2..10]

divisors :: Int -> [Int]
divisors x = filter (\m -> x `mod` m == 0) [2..(ceiling . sqrt .fromIntegral) x]

firstDivisors :: String -> Maybe String
firstDivisors str = if [] `elem` ds then Nothing else return $ (unwords . map (show . head)) ds where
  ds = map divisors $ bases str
