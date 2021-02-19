module Block (
    Block,
    initBlock,
    initBlocks
)where

import Coord

data Block = Block (Int,Int) Value Mine Visible SrndBlock deriving (Show)

newtype Visible = Visible Bool deriving (Show) 
newtype Mine = Mine Bool deriving (Show) 
newtype Value = Value Int deriving (Show)
newtype SrndBlock = SrndBlock [Block] deriving (Show)

initBlock :: (Int,Int) -> Block
initBlock x = Block x (Value 0) (Mine False) (Visible False) (SrndBlock [])

initBlocks :: [(Int,Int)] -> [Block]
initBlocks = map initBlock
