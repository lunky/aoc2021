module Day9
    ( 
    day9
   ,day9b
    )
    where
import AocLib
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.List (unfoldr, nub, sortOn, group)
import Data.Ord
    
day9 :: String -> Int 
day9 input = sum $ day9' input

day9' input = mapMaybe (\y-> testLower (fst y) grid) $ Map.toList grid
  where grid = parseInput input

testLower :: (Int,Int) -> Map (Int,Int) Int -> Maybe Int
testLower item grid = case Map.lookup item grid of   
                        Just x -> if all (\(a,b)-> b>x) adjacent then  Just (x+1) else Nothing
                        _ -> Nothing
  where adjacent = getAdjacent item grid

day9b :: String -> Int
day9b input = product $ take 3 $ reverseSort
                $ (\m-> map (\xy-> length $ getBasin xy [] m) $  day9b' m) 
                $ parseInput input

reverseSort = sortOn Data.Ord.Down

day9b' :: Map (Int,Int) Int -> [(Int,Int)]
day9b' grid = mapMaybe (\y-> testLowerb (fst y) grid) $ Map.toList grid

testLowerb :: (Int,Int) -> Map (Int,Int) Int -> Maybe (Int,Int)
testLowerb item grid = case Map.lookup item grid of   
                        Just x -> if all (\(a,b)-> b>x) adjacent then Just item else Nothing
                        _ -> Nothing
  where adjacent = getAdjacent item grid

mapLookup :: Ord k => k -> Map k a -> Maybe (k,a)
mapLookup k set = case Map.lookup k set of
                    Just x -> Just (k,x)
                    _ -> Nothing

getAdjacent :: (Int,Int) -> Map (Int,Int) Int -> [((Int,Int),Int)]
getAdjacent (x,y) grid = mapMaybe (`mapLookup` grid) [ (x,y+1), (x-1,y), (x+1,y), (x,y-1) ]

getBasin :: (Int,Int) -> [((Int,Int),Int)] -> Map (Int,Int) Int -> [((Int,Int),Int)]
getBasin point seen grid = nub $ (\y->y ++ concatMap (\(xy,p)->getBasin xy (y++seen) grid) y)
                        $ filter (\(_,payload)->payload/=9) 
                        $ filter (\(xy,_)-> notElem xy $ map fst seen)
                        $ getAdjacent point grid  

parseInput input = Map.fromList $ map (\(a,b)->(a,digitToInt b)) $ grid $ lines input
_input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
