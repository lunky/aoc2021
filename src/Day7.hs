module Day7
    ( 
    day7
   ,day7b
    )
    where

import Data.List
import Data.List.Split
    
day7 :: String -> Int 
day7 input = sum $ map (\y -> abs$(y-med)) unsorted
  where unsorted = parseInput input
        med = median unsorted

day7b :: String -> Int
day7b input = minimum $ day7b' input

day7b' input =  possible
  where unsorted = parseInput input
        (lower,upper)= (\y->(minimum y, maximum y)) unsorted
        possible=map (\start-> sum $ map (\y-> cost $ abs(y-start)) unsorted) [lower..upper]

cost :: Int -> Int
--cost x = sum [1..x]
cost n = n * (n + 1) `div` 2

_input = "16,1,2,0,4,2,7,1,2,14"

median :: [Int] -> Int
median input 
  | odd sortedLength = sorted !! (sortedLength `div` 2)
  | otherwise = ((sorted !! (sortedLength `div` 2)) 
               + (sorted !! ((sortedLength `div` 2)-1)))  `div` 2
    where sorted = sort input
          sortedLength = length sorted

parseInput :: String ->[Int]
parseInput = map read . splitOn "," 

--

