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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Char (digitToInt)
import Data.List (sortOn,foldl')
import Data.Ord
    
type Point = (Int,Int)
type Height = Int
type HeightMap = Map Point Height

day9 :: String -> Int 
day9 input = sum $ day9' input

day9' input = mapMaybe (\y-> testLower (fst y) grid) $ Map.toList grid
  where grid = parseInput input

testLower :: Point -> HeightMap -> Maybe Int
testLower item grid = case Map.lookup item grid of   
                        Just x -> if all (\(a,b)-> b>x) adjacent then  Just (x+1) else Nothing
                        _ -> Nothing
  where adjacent = getAdjacent item grid


day9b :: String -> Int
day9b input = product $ take 3 $ reverseSort
                $ (\m-> map (length . getBasin m ) $ lowPoints m) 
                $ parseInput input


reverseSort :: [Int] -> [Int]
reverseSort = sortOn Data.Ord.Down

lowPoints :: HeightMap -> [Point]
lowPoints grid = mapMaybe (\y-> testLower (fst y) grid) $ Map.toList grid
    where testLower item grid = case Map.lookup item grid of   
                                  Just x -> if all (\(a,b)-> b>x) adjacent then Just item else Nothing
                                  _ -> Nothing
            where adjacent = getAdjacent item grid

mapLookup :: Ord k => k -> Map k a -> Maybe (k,a)
mapLookup k set = case Map.lookup k set of
                    Just x -> Just (k,x)
                    _ -> Nothing

getAdjacent :: Point -> Map Point Height -> [(Point, Height)]
getAdjacent (x,y) grid = mapMaybe (`mapLookup` grid) [ (x,y+1), (x-1,y), (x+1,y), (x,y-1) ]

getBasin :: HeightMap -> Point -> Set Point
getBasin grid = getBasin' Set.empty
    where getBasin' seen point = foldl' haveSeen (Set.insert point seen) (getAdjacent point grid)
          haveSeen seen (point,height) = 
                if Set.member point seen || (height >= 9)
                then seen 
                else getBasin' seen point

parseInput :: String -> Map Point Height
parseInput input = Map.fromList $ map (\(a,b)->(a,digitToInt b)) $ grid $ lines input

_input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
