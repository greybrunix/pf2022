module FiftyQuestions where

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' n m | n > m = []
               | otherwise = n:enumFromTo (n+1) m

enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' start next end
    | start > end && next - start > 0 || start < end && next - start < 0 = []
    | otherwise = start:enumFromThenTo' next (2 * next - start) end
    -- this one is too scary sorry

plusplus :: [a] -> [a] -> [a]
plusplus [] l = l
plusplus l [] = l
plusplus (x:xs) (h:t) = x:(plusplus xs (h:t))

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

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (a:b) = (h,a):(zip t b)

replicate' :: Int -> a -> [a]
replicate' n r = if n > 0 then r:(replicate' (n-1) r) else []

intersperse' :: a -> [a] -> [a]
intersperse' _ [] = []
intersperse' _ [x] = [x]
intersperse' n (x:xs) = x:([n]++intersperse' n xs)

group' :: Eq a => [a] -> [[a]]
group' [] = []
group' [x] = [[x]]
group' (x:xs) | x == head (head lr) = (x: head lr) : (tail lr) -- note on this head of head further down (1)
              | otherwise = [x] : (head lr) : (tail lr)
              where lr = group' xs
-- (1) this is a list of lists so the head of the list of lists is a list, we can't compare an element to a list

concat' :: [[a]] -> [a]
concat' [] = []
concat' [[x]] = [x]
concat' (x:xs) = (aux [] x) ++ concat' xs
                where aux acc []     = acc
                      aux acc (h:[]) = acc ++ [h]
                      aux acc (h:t)  = aux (acc ++ [h]) t
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' x (h:t)
    | x == h = t
    | otherwise = h:delete' x t

remove :: Eq a => [a] -> [a] -> [a]
remove l [] = l
remove [] _ = []
remove l (h:t) = remove (delete' h l) t

pGreater :: Ord a => [a] -> Int
pGreater [_] = 0
pGreater (h:t) | h > ((!!) t x) = 0 -- (2) 
               | otherwise = 1 + x
               where x = pGreater t
               -- (2) x is the index of the greatest element in t so the bang bang function will get hte greatest elem in t
         
