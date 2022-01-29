module HOF where
import Data.List

-- general pre defined HOF
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
deleteBy' _ _ [] = []
deleteBy' f n (x:xs)  = if f n x then xs
                        else x:(deleteBy' f n xs)

sortOn' :: (Ord a, Ord b) => (a -> b) -> [a] -> [a]
sortOn' _ []     = []
sortOn' f (x:xs) = insert x (sortOn' f xs)

-- HOF over R[x]

type Polynomial = [Monomial]
type Monomial = (Float, Int)

seldeg :: Int -> Polynomial -> Polynomial
seldeg n p = filter (\x -> n == snd x) p

count :: Int -> Polynomial -> Int -- count 4 polynomialOne = 2
count n p = length $ filter (\x -> n == snd x) p

deg :: Polynomial -> Int
deg = foldl (\acc x-> if acc > snd x then acc else snd x) 0


deriv :: Polynomial -> Polynomial
deriv p = filter (/= (0,0)) $ map(\(b,e) -> if e >= 1 then (b * fromIntegral(e), e - 1) else (0,0)) p

calculate :: Float -> Polynomial -> Float
calculate x = foldl (\acc (c,e) -> acc + c * (x^e)) 0  

simp :: Polynomial -> Polynomial
simp p = filter (\(b,_) -> b /= 0) p



