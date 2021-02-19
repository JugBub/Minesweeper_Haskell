module Misc (
    askForInt,
    ioStrToInt,
    joinIoStr,
    pairIoStr,
    listIsInt,
)where

import Data.Maybe
import Text.Read

askForInt :: Int -> Int  -> String -> IO String
askForInt low high s = do
    putStrLn s
    x <- getLine 
    let checkX = readMaybe x :: Maybe Int
    if Just low <= checkX && checkX <= Just high && isJust checkX
    then return x
    else askForInt low high s
{-
purifyList :: [IO String] -> [String]
purifyList [] = []
purifyList (x:xs) = do
    y <- x
    let ys = y : purifyList xs
    return ys
-}

ioStrToInt :: IO String -> IO Int 
ioStrToInt x = do
    s <- x
    let i = read s 
    return i

joinIoStr :: IO String -> IO String -> IO String
joinIoStr s1 s2 = do
    cs1 <- s1
    cs2 <- s2
    let cs = cs1 ++ cs2
    return cs

pairIoStr :: IO String -> IO String -> IO String
pairIoStr s1 s2 = do
    cs1 <- s1
    cs2 <- s2
    let cs = cs1 ++ "\n" ++cs2
    return cs

checkIfInt :: String -> Int -> Int -> Bool
checkIfInt s low high = isJust i && Just low <= i && i <= Just high
    where i = readMaybe s

listIsInt :: [String] -> Int -> Int -> Bool
listIsInt [x] low high = checkIfInt x low high 
listIsInt (s:ss) low high = checkIfInt s low high && listIsInt ss low high

