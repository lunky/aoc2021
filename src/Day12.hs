module Day12
    ( 
    day12
   ,day12b
    )
    where
import Data.List.Split
import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Char (isUpper, isLower)
    
type Input = Map CaveNode [CaveNode]

type PathPredicate = Path -> CaveNode -> Bool
type CaveNode = String
type Path = [CaveNode]

day12 :: String -> Int 
day12 input = day12' pred $ parseInput input
  where pred path cave = cave `notElem` path || all isUpper cave


parseInput :: String -> Input
parseInput = Map.fromListWith (++) . 
              concatMap ((\[from, to] -> [(from, [to]), (to, [from])]) . splitOn "-") . lines


day12b :: String -> Int
day12b input = day12' pred $ parseInput input
  where
    hasDouble = any (\xs -> length xs > 1) . group . sort . filter (isLower . head)
    pred :: PathPredicate
    pred path cave = case cave of
      "end" -> cave `notElem` path
      "start" -> cave `notElem` path
      _ -> not (hasDouble path) || cave `notElem` path || all isUpper cave

exploreCaves :: Input -> PathPredicate -> CaveNode -> Path -> [Path] -> [Path]
exploreCaves caveMap pred currCave explored pathsToEnd = if currCave == "end"
                                                         then [currCave:explored]
                                                         else pathsToEnd ++ goodPaths
  where
    allowedExplore :: [CaveNode]
    allowedExplore = filter (pred (currCave:explored)) $ caveMap Map.! currCave
    goodPaths :: [Path]
    goodPaths = concatMap (\cave -> exploreCaves caveMap pred cave (currCave:explored) pathsToEnd) allowedExplore

day12' :: PathPredicate -> Input -> Int
day12' pred caveMap = length $ exploreCaves caveMap pred "start" [] []


_input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"
_input2 = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"

