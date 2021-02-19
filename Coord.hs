module Coord (
    toCoord,
    listsToCoords,
    lToCoord,
)where

data Coord = Coord Int Int deriving (Show)

toCoord :: Int -> Int -> (Int,Int)
toCoord x y = (x,y)

lToCoord :: [Int] -> (Int,Int)
lToCoord []         = (0,0)
lToCoord [x]        = (x,x)
lToCoord [x,y]      = (x,y)
lToCoord (x:y:ys)   = (x,y)

listsToCoords :: [Int] -> [Int] -> [(Int, Int)]
listsToCoords [] _          = []
listsToCoords _ []          = []
listsToCoords (x:xs) (y:ys) = (x,y) : listsToCoords xs ys