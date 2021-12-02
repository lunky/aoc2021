module Day2
    ( 
    day2
   ,day2b
   ,move
   ,move2
    )
    where

day2 :: String -> Int 
day2 input = uncurry (*) $ foldr move (0,0) $ parseInput input

move :: Num a => (String, a) -> (a, a) -> (a, a)
move ("forward", forward) (x,y) = (x+forward, y)
move ("up", up) (x,y) = (x, y - up)
move ("down", down) (x,y) = (x, y + down)
move (_,_) (_,_) = error "unexpected value"

----
day2b :: String -> Int
day2b input = (\(a,b,_)->a*b) $ foldr move2 (0,0,0) $ reverse $ parseInput input

move2 :: Num a => (String, a) -> (a, a, a) -> (a, a, a)
move2 ("forward", forward) (x,y,aim) = (x+forward, y + (forward*aim), aim)
move2 ("up", up) (x,y,aim) = (x, y, aim - up)
move2 ("down", down) (x,y,aim) = (x, y, aim + down)
move2 (_,_) (_,_,_) = error "unexpected value"

parseInput :: String -> [(String,Int)]
parseInput input = map ((\[a,b]->(a,read b)).words)$ lines input

_input = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

