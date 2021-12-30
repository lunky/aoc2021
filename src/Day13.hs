{-# LANGUAGE TupleSections #-}
module Day13
    ( 
    day13
   ,day13b
   -- ,foldy,foldy'
    )
    where
    
import Data.List.Split
import Data.List (groupBy,sort,sortBy)

import Data.Map (Map)
import qualified Data.Map as Map

data FoldRule = FoldRule Char Int
                  deriving Show
type Coords = (Int,Int) 
type Grid = Map Coords Char

day13 :: String -> Int 
day13 input = length $ filter (=='#') $ map snd $ Map.toList $ day13' input

day13' input = foldr applyRules (toGrid dots) (take 1 rules)
  where parsed = parseInput input
        (dots,rules) = parsed

day13b' input = foldr applyRules (toGrid dots) rules
  where parsed = parseInput input
        (dots,rules) = parsed

applyRules (FoldRule 'x' x) acc = foldx x acc
applyRules (FoldRule 'y' y) acc = foldy y acc

-- foldy yValue grid = upper `foldUnion` lower
--   where upper = Map.filterWithKey (\(x,y) v-> y < yValue) grid
--         lower = Map.fromList $ map (\(k,v)->yTranslate yValue k v) $ Map.toList $ Map.filterWithKey (\(x,y) v-> y >= yValue) grid

{--
  -- todo replace Map.tolist Map.fromList above to an implace operation with a Map.mapWithKey?
  --}
foldy yValue grid = upper `foldUnion` lower
  where upper = Map.filterWithKey (\(x,y) v-> y < yValue) grid
        lower = Map.foldrWithKey (\(x,y) v acc -> 
                             if y > yValue then 
                               (\(key,value) -> Map.insert key value acc ) $ yTranslate yValue (x,y) v
                             else acc ) Map.empty grid


yTranslate yValue (x,y) v =  ((x, y - ((y - yValue)*2)),v)
xTranslate xValue (x,y) v =  ((x - ((x - xValue)*2),y),v)

foldx xValue grid = left `foldUnion` right 
  where left = Map.filterWithKey (\(x,y) v-> x < xValue) grid
        right = Map.fromList $ map (\(k,v)->xTranslate xValue k v) $ Map.toList $ Map.filterWithKey (\(x,y) v-> x >= xValue) grid

foldUnion :: Map Coords Char -> Map Coords Char -> Map Coords Char 
foldUnion = Map.unionWith clobber 
  where 
    clobber '.' '#' = '#'
    clobber '#' '.' = '#'
    clobber '.' '.' = '.'
    clobber '#' '#' = '#'


--day13b :: String -> IO 
day13b input = _showGrid $ day13b' input

--toGrid :: [Coords] -> [(Coords,Char)]
toGrid xs = Map.fromList (map (,'#') xs) 
    `Map.union`  Map.fromList [ ((x,y),'.') | y <- [0..yUpper], x<-[0..xUpper]]
  where (xUpper,yUpper) = bounds xs
  
toGrid' xs = bounds' xs
  where (xLower, yLower, xUpper,yUpper) = bounds' xs
  
parseInput :: String -> ([Coords],[FoldRule])
parseInput input = (\[a,b]->(map toCoord $ lines a,map toRule $ lines b))$ splitOn "\n\n" input
  where toCoord x = (\[a,b]-> (read a::Int, read b::Int) ) $ splitOn "," x
        toRule :: String -> FoldRule
        toRule x = (\[a,b]-> FoldRule (head a) (read b) ) $ splitOn "=" $ drop (length "fold along ") x

_showGrid grid = mapM_ (putStrLn . map snd) 
             $ groupBy (\(xy,_) (xy2,_) -> snd xy == snd xy2) 
             $ _sortCoord  
             $ Map.toList grid
--_showGrid' grid = 

bounds :: [Coords] -> (Int,Int)
bounds grid = (maximum $ map fst grid, maximum $ map snd grid)
  
bounds' :: [Coords] -> (Int,Int,Int,Int)
bounds' grid = (minimum$ map fst grid, minimum $ map snd grid, maximum $ map fst grid, maximum $ map snd grid)


_sortCoord :: (Ord a1, Ord a2) => [((a2, a1), b)] -> [((a2, a1), b)]
_sortCoord = sortBy (\((a,b),c) ((a2,b2),c2) -> compare b b2 <> compare a a2)


_input = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5"
