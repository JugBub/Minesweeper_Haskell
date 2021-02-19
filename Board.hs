module Board(
    Board,
    sqBoard,
    rectBoard,
    fillBoard,
)where

import Coord
import Block

data Board = RctBoard Int Int | SqBoard Int | Board Int Int [Block] deriving (Show)

rectBoard :: Int -> Int -> Board
rectBoard = RctBoard

sqBoard :: Int -> Board
sqBoard = SqBoard

fillBoard :: Board -> Board
fillBoard (Board w h bs) = Board w h bs
fillBoard (SqBoard b) = fillBoard $ RctBoard b b
fillBoard (RctBoard w h) = Board w h $ initBlocks $ listsToCoords ws hs
    where   ws = map (`mod` w) [1,2..(w*h)]
            hs = concatMap (replicate w) [1,2..h]

