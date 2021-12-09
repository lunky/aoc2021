module Day8
    ( 
    day8
   ,day8b
    )
    where
import Data.List
import Data.List.Split
import Data.Maybe
    
day8 :: String -> Int 
day8 input = length 
                $ concatMap catMaybes 
                $ map (\u -> map lengthTest $ snd u) 
                $ parseInput input

day8b :: String -> Int
day8b input = sum $ map (\y-> read $ concatMap show y) $ map day8b' $ parseInput input

test input =  (reference, map (\y-> (lookup (sort y))) $ concatMap snd rows)
  where rows = parseInput input
        lookup el = snd $ head $ filter (\(a,b)->a==el) reference
        reference = zip (map sort $ day8b'' $ head $ parseInput input) [0..]

day8b' input = map (\y->lookup $ sort y) $ snd input
  where
        lookup el = snd $ head $ filter (\(a,b)->a==el) reference
        reference = zip (map sort $ day8b'' input) [0..]

day8b'' (patterns, values) = [zero,one,two,three,four,five,six,seven,eight,nine]
  where 
    Just zero = find (\y->length y==6 &&  y/=nine && y/=six) patterns
    Just one = find (\y->length y==2) patterns
    Just two = find (\y->length y == 5 && length (eight \\ (nub $ four ++ y) ) == 0) patterns
    Just three = find (\y->length y == 5 && length (y \\ one) == 3) patterns
    Just four = find (\y->length y==4) patterns
    Just five = find (\y->length y==5 && length (eight \\ (nub $ two ++ y)) ==0) patterns
    Just six = find (\y->length y==6 && length (eight \\ nub (one++y) ) == 0) patterns
    Just seven = find (\y->length y==3) patterns
    Just eight = find (\y->length y==7) patterns
    Just nine = find (\y->length y==6 && length (y \\ (nub $ four ++ five)) == 0) patterns
  

parseInput input = map ((\[a,b]->(words a,words b)). splitOn " | ") $ lines input

lengthTest input
  | length input == 2 = Just 1
  | length input == 4 = Just 4
  | length input == 3 = Just 7
  | length input == 7 = Just 8
  | otherwise = Nothing

_input = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\nedbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\nfgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\nfbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\naecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\nfgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\ndbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\nbdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\negadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\ngcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n"
_input2 = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
_input3 ="fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"

_ref= [
    ("abcdeg",0),
    ("ab",1),
    ("acdfg",2),
    ("abcdf",3),
    ("abef",4),
    ("bcdef",5),
    ("bcdefg",6),
    ("abd",7),
    ("abcdefg",8),
    ("abcdef",9)
  ]

