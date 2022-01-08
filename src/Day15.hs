module Day15
    (
    day15
   ,day15b
   ,_input
    )
    where
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Algorithm.Search (dijkstra)
import Data.Ix (range, inRange)
import Data.Bifunctor (bimap)

type Point = (Int, Int)


start :: Point
start = (0,0)

adjacent :: (Point,Point) -> Point -> [Point]
adjacent bounds (r,c) = filter (inRange bounds) $ map (bimap (+r) (+c)) [ (-1,0), (1,0), (0,-1), (0,1) ]

day15 :: String -> Int
day15 input = fst $ fromJust $ dijkstra (adjacent bounds) cost' end start
  where
    cost i = grid Map.! i
    cost' _ = cost 
    bound = length $ lines input
    bounds = (start,(bound-1,bound-1)) 
    end = (== snd bounds)
    grid = (Map.fromList . zip (range bounds) . map digitToInt . concat . lines) input

day15b :: String -> Int
day15b input = fst $ fromJust $ dijkstra (adjacent bounds2) cost' end start
 where
   grid  = (Map.fromList . zip (range bounds) . map digitToInt . concat . lines) input
   bound = length $ lines input
   bounds = (start,(bound-1,bound-1)) 
   bounds2 = (\y->(start, (y*5-1,y*5-1))) bound
   end = (== snd bounds2)
   idx (r,c) = (r`mod`bound, c`mod`bound)
   dist (r,c) = r`div`bound + c`div`bound
   cost' _  = cost
   cost i = let c = grid Map.! idx i + dist i in
                if c > 9
                   then c - 9
                   else c

_input = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"

-- 2922
-------------------------------------------------------------------------------
