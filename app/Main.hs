{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-do-bind #-}
module Main where

import Day1
import Day2
import Day3
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

    contents <- readFile "data/day2.txt"
    let answer = show $ day2 contents
    putStrLn ("day2: " ++ answer)
    let answer = show $ day2b contents
    putStrLn ("day2b: " ++ answer)

    contents <- readFile "data/day3.txt"
    let answer = show $ day3 contents
    putStrLn ("day3: " ++ answer)
    let answer = show $ day3b contents
    putStrLn ("day3b: " ++ answer)
