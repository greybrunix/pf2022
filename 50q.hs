module FiftyQuestions where

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' n m | n > m = []
               | otherwise = n:enumFromTo (n+1) m

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start next end
    | start > end && next - start > 0 || start < end && next - start < 0 = []
    | otherwise = start:enumFromThenTo' next (2 * next - start) end
    -- this one is too scary sorry

concat' :: [a] -> [a] -> [a]
concat' [] l = l
concat' l [] = l
concat' (x:xs) (h:t) = x:(concat' xs (h:t))

indexElem :: [a] -> Int -> a
indexElem [] _ = error"This position does not exist"
indexElem (x:xs) n = if n == 0 then x else indexElem xs (n-1)

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse t ++ [h]

take' :: Int -> [a] -> [a]
take' _ [] = []
take' 0 l = []
take' n (x:xs) = x:(take' (n-1) xs)

drop' :: Int -> [a] -> [a]
drop' _ [] = []
drop' 0 l = l
drop' n (x:xs) = drop' (n-1) xs
