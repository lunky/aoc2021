module Day6
    ( 
    day6
   ,day6b
    )
    where
import Data.List.Split (splitOn)
    
day6 :: String -> Int 
day6 input = length $ 
               iterate (
                   foldr (\y acc -> if y==0 then 8:(6:acc) else y-1:acc) []
                 ) (parseInput input) !! 80

emptyBuckets = [0,0,0,0,0,0,0,0,0] 

day6b :: Int -> String -> Int
day6b days input = sum $ head $ drop days 
                   $ iterate (\(x:xs) -> replace 6 (xs!!6 + x) xs ++ [x]) 
                   $ foldr (\y acc-> replace y (acc!!y +1) acc) emptyBuckets
                   $ parseInput input

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

_input = "3,4,3,1,2"

replace pos newVal list = take pos list ++ newVal : drop (pos+1) list
