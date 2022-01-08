module AocLib
    ( someFunc
      , doubleFunc
      , grid
      , adjacent
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

doubleFunc :: Int -> Int
doubleFunc x = x + x

grid :: [[a]] -> [((Int,Int), a)]
grid = concat . zipWith (curry grid') [1..]
  where
    grid' (y,xs) = map (\(x,datum)-> ((x,y),datum)) $ zip [1..] xs

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) = [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]
