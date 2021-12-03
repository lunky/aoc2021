module Day3
    ( 
    day3
   ,day3b
    )
    where
import Data.List (group, transpose, sort)
    
day3 :: String -> Int 
day3 input = bintodec (gamma input) * bintodec (epsilon input)

gamma :: String -> Int
gamma input = read $ map (head.longest.group.sort) $ transpose $ parseInput input

mostCommon :: Ord c => [c] -> c
mostCommon = head.longest.group.sort

leastCommon :: Ord c => [c] -> c
leastCommon = head.shortest.group.sort

epsilon :: String -> Int
epsilon input = read $ map (head.shortest.group.sort) $ transpose $ parseInput input

longest xs = snd $ maximum $ map (\x -> (length x, x)) xs
shortest xs = snd $ minimum $ map (\x -> (length x, x)) xs


day3b :: String -> Int
day3b input = bintodec (read $ o2rating 0 (parseInput input)) *
              bintodec (read $ co2rating 0 (parseInput input))

o2rating :: Int -> [String] -> String
o2rating _ [x] = x
o2rating offset xs = o2rating (offset+1) $ filter (\x -> x !! offset == most) xs
  where most = mostCommon $ transpose xs !! offset

co2rating :: Int -> [String] -> String
co2rating _ [x] = x
co2rating offset xs = co2rating (offset+1) $ filter (\x -> x !! offset == most) xs
  where most = leastCommon $ transpose xs !! offset


{--
parseInput' :: String -> [[Int]]
parseInput' input = map (map read.words) $ lines input
_input = "1 2 3 4\n5 6 7 8\n9 10 11 12\n13 14 15 16"
--}
_input = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010"


parseInput = lines 

bintodec :: Integral i => i -> i
bintodec 0 = 0
bintodec i = 2 * bintodec (div i 10) + mod i 10
