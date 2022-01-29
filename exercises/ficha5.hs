module HOF where

any' :: (a -> Bool) -> [a] -> Bool
any' _ []     = False
any' f (x:xs) = f x && any' f xs

zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _        = []
zipWith' _ _ []        = []
zipWith' f (h:t) (a:b) = (f h a): zipWith' f t b

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' _ []     = []
takeWhile' f (x:xs) = if f x then x:(takeWhile' f xs) else []  

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' f (x:xs) | f x       = (dropWhile' f xs)
                    | otherwise = x:xs

span' :: (a->Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span f (x:xs) | f x = (x:aux1,aux2)
              | otherwise = ([],x:xs)
        where (aux1,aux2) = span' f xs

deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
