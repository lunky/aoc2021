{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}
module Main where

import Day1
import Data.Time

timeStamp :: IO a -> IO ()
timeStamp f = do 
    c <- getCurrentTime                  --  2009-04-21 14:25:29.5585588 UTC 
    print c
    f
    c <- getCurrentTime                  --  2009-04-21 14:25:29.5585588 UTC 
    print c


main :: IO ()
main = 
    timeStamp runDays

runDays :: IO ()
runDays = do
    contents <- readFile "data/day1.txt"
    let answer = show $ day1 contents
    putStrLn ("day1: " ++ answer)
    let answer = show $ day1b contents
    putStrLn ("day1b: " ++ answer)
