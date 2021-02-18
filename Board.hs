module Board(
    Board,
    sqBoard,
    rectBoard,
    fillBoard,
)where

import Coord
import Block

data Board = RctBoard Int Int | SqBoard Int | Board [Block] deriving (Show)

rectBoard :: Int -> Int -> Board
rectBoard = RctBoard

sqBoard :: Int -> Board
sqBoard = SqBoard

fillBoard :: Board -> Board
fillBoard (Board bs) = Board bs
fillBoard (SqBoard b) = fillBoard $ RctBoard b b
fillBoard (RctBoard w h) = Board $ initBlocks $ lsToCoords ws hs
    where   ws = map (`mod` w) [1,2..(w*h)]
            hs = concatMap (replicate w) [1,2..h]

