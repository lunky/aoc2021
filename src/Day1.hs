module Day1
    (  day1
      ,day1b
    ) where

day1 :: String -> Int
day1 input =  length $ filter (==True) $ zipWith (<) series (tail series)
  where series = parseInput input

day1b :: String -> Int
day1b input =length $ filter (==True) $ zipWith (<) series (tail series) 
 where series = zipWith3 (\a b c -> a+b+c) parsed (tail parsed) (drop 2 parsed)
       parsed = parseInput input

parseInput :: String -> [Int]
parseInput input = map read $ lines input

_input = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"
