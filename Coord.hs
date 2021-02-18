module Coord (
    Coord,
    toCoord,
    lsToCoords,
)where

data Coord = Coord Int Int deriving (Show)

toCoord :: Int -> Int -> Coord
toCoord = Coord

lsToCoords :: [Int] -> [Int] -> [Coord]
lsToCoords [] _ = []
lsToCoords _ [] = []
lsToCoords (x:xs) (y:ys) = Coord x y : lsToCoords xs ys