module Day11
    ( 
    day11
   ,day11b
    )
    where
import Data.List
import Data.List.Split
import Data.Char
import AocLib
import Data.Map (Map)
import qualified Data.Map as Map

data Octo = Octo Int Bool deriving (Show, Eq, Ord)
    
day11 :: String -> Int 
day11 input = fst $ iterate steps (0,Map.fromList $ parseInput input) !! 100


steps :: (Int,Map (Int,Int) Octo) -> (Int,Map (Int,Int) Octo)
steps (current,input) = (\(next,res)->(current+next,res))
                $ step3 
                $ head 
                $ dropWhile (not . null . Map.filter (\(Octo v f)->v>9)) 
                $ iterate step2
                $ step1 input

stepsb :: (Int,Int,Map (Int,Int) Octo) -> (Int,Int,Map (Int,Int) Octo)
stepsb (index,current,input) = (\(a,b)->(index+1,a,b))
                $ step3
                $ head 
                $ dropWhile (not . null . Map.filter (\(Octo v f)->v>9)) 
                $ iterate step2
                $ step1 input

day11b :: String -> Int
day11b input = (\(a,b,c)->a)
                  $ head  
                  $ dropWhile (\(_,a,_)->a/=total) 
                  $ iterate stepsb (0,0,Map.fromList parsed)
  where parsed = parseInput input  
        total = length parsed

step1 :: Map k Octo -> Map k Octo
step1 =  Map.map (\(Octo y f) -> Octo (y+1) f) 

step2 :: Map (Int,Int) Octo -> Map (Int,Int) Octo
step2 xs = Map.foldrWithKey flash  xs xs

step3 :: Map (Int,Int) Octo -> (Int,Map (Int,Int) Octo)
step3 xs =  (flashed,result) 
  where flashed = Map.size $ Map.filter (\(Octo v canFlash)-> not canFlash) xs
        result = Map.foldrWithKey(\k (Octo v canFlash) acc -> 
                    if not canFlash then 
                      Map.insert k (Octo 0 True) acc
                    else acc ) xs xs

flash :: (Int,Int) -> Octo -> Map (Int,Int) Octo -> Map (Int,Int) Octo 
flash k (Octo v f) acc 
  | v > 9 && f = Map.insert k (Octo 0 False) $ bumpAdjacent k acc 
  | otherwise = acc

bump :: (Int, Int) -> Map (Int,Int) Octo -> Map (Int,Int) Octo 
bump key acc = if Map.member key acc 
               then Map.insertWith (\(Octo a f) (Octo b f2) -> 
                    Octo (a+b) (f && f2)) key (Octo 1 True) acc 
               else acc

bumpAdjacent :: (Int,Int) -> Map (Int,Int) Octo -> Map (Int,Int) Octo
bumpAdjacent (x,y) acc = foldr bump acc $ getAdjacent' (x,y)
  where
    getAdjacent' (x,y) = [ (x-1,y-1), (x,y-1), (x+1,y-1),
                           (x-1,y),            (x+1,y),
                           (x-1,y+1), (x,y+1), (x+1,y+1)
                         ]

{-# ANN parseInput "HLint: ignore Avoid lambda" #-}
parseInput :: String -> [((Int, Int), Octo)]
parseInput = grid . map ( map ((\y->Octo y True).digitToInt)) . lines 

_input :: String
_input = "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"

_input2 :: String
_input2 = "11111\n19991\n19191\n19991\n11111"

_unGrid :: [a] -> [[a]]
_unGrid xs = chunksOf ( floor $ sqrt $ fromIntegral $ length xs) xs  -- depends on sort order

_showGrid :: (Ord a1, Ord a2) => [((a1,a2),Octo)] -> IO ()
_showGrid xs = mapM_ print $ _unGrid $ map ( (\(Octo v f) -> v) . snd) $ _sortCoord xs

_showGrid' :: (Ord a1, Ord a2) => [((a1,a2),Octo)] -> IO ()
_showGrid' xs = mapM_ print $ _unGrid $ map snd $ _sortCoord xs

_sortCoord :: (Ord a1, Ord a2) => [((a2, a1), b)] -> [((a2, a1), b)]
_sortCoord = sortBy (\((a,b),c) ((a2,b2),c2) -> compare b b2 <> compare a a2)
