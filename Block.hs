module Block (
    Block,
    initBlock,
    initBlocks
)where

import Coord

data Block = Block Coord Value Mine Visible SrndBlock deriving (Show)

newtype Visible = Visible Bool deriving (Show) 
newtype Mine = Mine Bool deriving (Show) 
newtype Value = Value Int deriving (Show)
newtype SrndBlock = SrndBlock [Block] deriving (Show)

initBlock :: Coord -> Block
initBlock c = Block c (Value 0) (Mine False) (Visible False) (SrndBlock [])

initBlocks :: [Coord] -> [Block]
initBlocks = map initBlock
