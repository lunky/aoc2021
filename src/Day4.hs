module Day4
    ( 
    day4
   ,day4b
    )
    where

import Data.List
import Data.List.Split
    

day4 :: String -> Int 
day4 input = (head $ snd winner) * (sum $ (concat $ fst winner) \\ (snd winner))
  where 
    (boards,bingoNumbers) = parseInput input
    winner = head $ filter isWinner $ head $ dropWhile (\u->not  (any isWinner u)) $ scanl (\acc y-> callNumber y acc) boards bingoNumbers

--day4b input = 0

day4b input =  (head $ snd winner) * (sum $ (concat $ fst winner) \\ (snd winner))
  where 
    winner = day4b' input

day4b' input =  winner
  where 
    (boards,bingoNumbers) = parseInput input
    winner = head 
               $ filter isLast 
               $ head 
               $ dropWhile (\u->not  (all isWinner u)) 
               $ scanl (\acc y-> callNumber y acc) boards bingoNumbers


_input = "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7"

parseInput input = (boards,bingoNumbers)
  where 
    rawInputLines = lines input
    (firstLine:rawBoardLines) = rawInputLines
    bingoNumbers = map (\y->read y::Int) $ splitOn "," firstLine
    boards = initializeBoards $ readBoards rawBoardLines

readBoards :: [String] -> [[[Int]]]
readBoards input =  chunksOf 5 $ map (\y -> map (\x->read x::Int) $ words y) $ filter (/="") input

updateBoardState (board,seen) next 
  | (elem next $ concat board)  == True = (board, (next:seen))
  | otherwise = (board,seen)

isWinner :: (Foldable t, Eq a) => ([[a]], t a) -> Bool
isWinner (board,seen) = 
      any (\y -> all (\x-> elem x seen) y) (board ++ transpose board)

isLast :: Eq a => ([[a]], [a]) -> Bool
isLast (board,seen) = 
      any (\y -> (elem (head seen) (y) )  && all (\x-> elem x seen) y) (board ++ transpose board)

callNumber number boards = map (\board -> updateBoardState board number) boards

initializeBoards boards = map (\y->(y,[])) boards
