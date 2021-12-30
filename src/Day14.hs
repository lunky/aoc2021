module Day14
    (
    day14
   ,day14b
   ,day14'
   ,pairs
    )
    where
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

day14 :: String -> Int
day14 input = (\y-> maximum y - minimum y ) $ freq $ day14' 10 input

day14b :: String -> Int
day14b input = (\y-> maximum y - minimum y ) $ freq $ day14' 40 input


freq :: Map String Int -> [Int]
freq m = map (\y->y `div` 2 + y `rem` 2) $ Map.elems  
              $ foldr (\(k,v) acc->Map.insertWith (+) k v acc) Map.empty
              $ concatMap (\([a,b],c)->[(a,c),(b,c)]) 
              $ Map.toList m 

day14' :: Int -> String -> Map String Int
day14' x input =  iterate go seedMap !! x
  where go :: Num a => Map String a -> Map String a 
        go m = foldr (\([x,y],v) acc-> 
                     case Map.lookup [x,y] rulesMap of
                       Just z -> Map.insertWith (+) [z,y] v 
                               $ Map.insertWith (+) [x,z] v 
                               acc
                       _ -> Map.insert [x,y] v acc
                     ) Map.empty (Map.toList m) 
        (seed,rules) = parseInput input
        seedMap = foldr ((\ x acc -> Map.insertWith (+) x 1 acc) . (\ (a, b) -> [a, b])) Map.empty (zip seed $ tail seed)
        rulesMap = Map.fromList rules

pairs :: String -> Map String Int
pairs seed = Map.fromList $ map (\((a,b),c)->([a,b],c)) $ Map.toList $ foldr (\y acc->Map.insertWith (+) y 1 acc) Map.empty (zip seed $ tail seed) 

parseInput :: String -> (String, [(String,Char)])
parseInput input = (polymerTemplate, map ((\[a,b]->(a, head b)) . splitOn " -> ") $ lines pairInsertionRules )
  where [polymerTemplate,pairInsertionRules]=  splitOn "\n\n" input

_input = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C"
