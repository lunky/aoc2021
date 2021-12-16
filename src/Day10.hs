module Day10
    ( 
    day10
   ,day10b
    )
    where
import Data.List
    
day10 :: String -> Int 
day10 input = sum $ map score $ concatMap (snd . day10') $ parseInput input 
  where day10' input = foldr process ([],[]) $ reverse input


process :: Char -> (String,String) -> (String,String)
process x (xs,err)
  | null xs = ([x],err)
  | x `elem` "({[<" = (x:xs,err)
  | x == opposite tip = (delete (opposite x) xs,err) 
  | otherwise =  (tail xs,x:err)
    where tip = head xs

opposite ')' = '('
opposite '}' = '{'
opposite ']' = '['
opposite '>' = '<'
opposite '(' = ')'
opposite '{' = '}'
opposite '[' = ']'
opposite '<' = '>'
opposite other = error ("invalid character data " ++ [other])


day10b :: String -> Int
day10b input = middle $ sort $ map scoreb $ day10b'' input
  where   
    day10b'' input = map (map opposite.fst) $ filter (\(a,b)->null b) $ map day10b' $ parseInput input
    day10b' input = foldr process ([],[]) $ reverse input


score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score other = error ("invalid character data " ++ [other])

scoreb :: String -> Int
scoreb = foldl' (\acc y -> (acc*5) + scoreb' y) 0
  where 
    scoreb' ')' = 1
    scoreb' ']' = 2
    scoreb' '}' = 3
    scoreb' '>' = 4

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)


parseInput = lines 
_input = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]"
