module Day5
    ( 
    day5
   ,day5b
   ,plotLine
    )
    where
import Data.List.Split
import Data.List
    
day5 :: String -> Int 
day5 input =  length $ filter (\y->length y>1) $group $ sort $ concatMap plotLine  $ day5' input 
day5' input = filter (\[(x1,y1),(x2,y2)]-> (x1==x2 || y1==y2)) $ parseInput input


plotLine [(x1,y1),(x2,y2)] 
 | x1==x2 || y1==y2 = [ (x,y) | x <- [(min x1 x2)..(max x1 x2)], y <- [(min y1 y2)..(max y1 y2)]]  
 | x1 < x2  && y1 < y2 = [ (x1 + x,y1 + x) | x <- [0..max x1 x2-min x1 x2]]
 | x1 > x2  && y1 > y2 = [ (x1 - x,y1 - x) | x <- [0..max x1 x2-min x1 x2]]
 | x1 < x2  && y1 > y2 = [ (x1 + x,y1 - x) | x <- [0..max x1 x2-min x1 x2]]
 | otherwise =           [ (x1 - x,y1 + x) | x <- [0..max x1 x2-min x1 x2]]


day5b :: String -> Int
day5b input =  length $ filter (\y->length y>1) $group $ sort $ concatMap plotLine  $ parseInput input 

parseInput :: String -> [[(Int,Int)]]
parseInput input = map (map ((\[a,b]->(read a,read b)) . splitOn ",") . splitOn " -> ") $ lines input

_input = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2"
