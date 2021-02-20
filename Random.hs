module Random (

    
)where

tupleToOp :: (Int,Int) -> Char 
tupleToOp (x,y)
    | a == 0 && b == 0 = '+'
    | a == 1 && b == 0 = '-'
    | a == 0 && b == 1 = '*'
    | a == 1 && b == 1 = '/'
    where   a = mod x 2
            b = mod y 2

middleSquareSequence :: Int -> Int -> [Int] 
middleSquareSequence 0 _ = []
middleSquareSequence it state = x : middleSquareSequence (it-1) x
    where x = middleSquare state

middleSquare :: Int -> Int
middleSquare state
    | even (length i) = digToInt $ takePartOfList i ((length i - n) `div` 2) n 
    | otherwise = digToInt $ takePartOfList i ((length i - n+1) `div` 2) (n+1) 
    where   n = length i `div` 2
            i = intToDig (state^2)

intToDig :: Int -> [Int]
intToDig 0 = []
intToDig x = intToDig (abs x `div` 10) ++ [mod (abs x) 10]

digToInt :: [Int] -> Int 
digToInt [] = 0
digToInt xs = x*10^exp + digToInt (tail xs)
    where   x = head xs
            exp = length xs -1

takePartOfList :: [a] -> Int -> Int -> [a]
takePartOfList _ _ 0  = [] 
takePartOfList [] _ _ = []
takePartOfList (x:xs) 0 a = x : takePartOfList xs 0 (a-1)
takePartOfList (x:xs) i a = takePartOfList xs (i-1) a