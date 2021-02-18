module Main where

import Board
import Misc

main :: IO ()
main = do
    tType' <- askForBoardType
    tSize <- askForBoardSize $ read tType'
    let type' = read tType' :: Int
        size  = map (\s -> read s :: Int) $ lines tSize
        board = fillBoard $ setBoard size
        
    print type'
    print size

game :: Board -> String -> IO ()
game board "Game Start" = do
game board "Gaming" = do
game board "Game Over" = do

askForCoordinate :: IO String
askForCoordinate = do
    putStrLn "Type the coordinate -- \"x y\""
    tS <- getLine
    let s = words tS
    if length s == 2 && 

askForBoardType :: IO String  
askForBoardType = askForInt 1 2 "Choose your type of board\n1 -- Square Board\n2 -- Rectangular Board"

askForBoardSize :: Int -> IO String
askForBoardSize 1 = askForSqBoardSize
askForBoardSize 2 = pairIoStr askForBoardWidth askForBoardHeight

askForSqBoardSize :: IO String
askForSqBoardSize = askForInt 1 maxBound "What's the board's side length"

askForBoardWidth :: IO String
askForBoardWidth = askForInt 1 maxBound "What's the board's width"

askForBoardHeight :: IO String
askForBoardHeight = askForInt 1 maxBound "What's the board's height"

setBoard :: [Int] -> Board
setBoard [x] = sqBoard x -- Singleton list must = SqBoard
setBoard [x,y] = rectBoard x y -- Binary list must = RctBoard