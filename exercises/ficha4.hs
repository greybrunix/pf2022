module Ficha04 where
import Data.List
import Data.Char
-- isAlpha, isDigit
digitAlpha :: String -> (String, String)
digitAlpha "" = ("", "")
digitAlpha (x:xs) = case (isAlpha x) of
                    True -> (x:y, z)
                    False -> (y, x:z)
              where (y,z) = digitAlpha xs

nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0,0,0)
nzp (a:as) 
           | a < 0   = (x+1,y,z)
           | a == 0  = (x,y+1,z)
           | otherwise = (x,y,z+1)
          where (x,y,z) = nzp as

divmod :: Integral a => a -> a -> (a,a)
divmod _ 0 = error "Division by zero"
divmod x y = foldl (\(a,b) n -> (a+1,b-y)) (0,x) [y,y*2..x]

fromDigits :: [Int] -> Int
fromDigits []    = 0
fromDigits (h:t) = h*10^(length t) + fromDigits t

fromDigitsOpt :: [Int] -> Int
fromDigitsOpt = foldl (\acc n -> n + 10 * acc) 0

maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit l  = maximum [sum m | m <- inits l]

maxSumInitOpt :: (Num a, Ord a) => [a] -> a
maxSumInitOpt l = maximum (foldl (\acc n -> (sum n):acc) [] (inits l))
--maxSumInitOpt l = maximum (aux [] [] (inits l))
  --             where aux :: (Num a, Ord a) => [a] -> [a] -> [[a]] -> [a]
    --                 aux acc l [] = (sum l):acc
      --               aux acc l il = aux (sum l:acc) (head il) (tail il)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibOpt :: Int -> Int
fibOpt n = aux n 0 1
        where aux 0 x _ = x
              aux n x y = aux (n-1) y (x+y)

intToStr :: Int -> String
intToStr n = aux "" n n
        where aux :: String -> Int -> Int -> String
              aux _ _ 0 = "0"
              aux acc 0 n = if (n < 0) then '-':acc else acc
              aux acc n m = aux ((chr ((mod (abs n) 10)+48)):acc) (div (abs n) 10) m

-- using the K&R itoa to make this, since we add to the head of the list we dont need to reverse

-- comprehension lists 
-- [x | x -> [1..20], mod x 2 == 0, mod x 3 == 0]
-- in other words, x is a number between 1 and 20 such that it is an even multiple of 3
-- [6, 12, 18]
-- Note that this is also equivalent to the following expression
-- [x | x <- [1..20], mod x 6 == 0]
-- due to simple modulo properties, trivial number theory
-- Even so let us attempt to create a function that does this for any range
--f :: Int -> Int -> [Int]
--f x y | x > y = [n | [y..x], mod n 6 == 0]
--      | otherwise = [n | [x..y], mod n 6 == 0]
-- let us try to replace these comprehension lists with filters
f :: Int -> Int -> [Int]
f x y | x > y = filter (\n -> mod n 6 == 0) [y..x]
       | otherwise = filter  (\n -> mod n 6 == 0) [x..y]

-- [x | x <- [y | y <- [1..20], mod y 2 == 0], mod x 3 == 0]
-- in other words we want all 'x's such that they are multiples of 3 that are in a list of 'y's
-- which are even numbers in the range of 1 to 20
-- [[2, 4, 6, 8, 10, 12, 14, 16, 18, 20], mod x 3]]
-- [6, 12, 18]
-- As we can see, this is the exact same as the previous list

fCL = [x | x <- [1..20], mod x 6 == 0]

-- [(x,y) | x <- [0..20], y <- [0..20], x+y == 30]
-- to simplify, we want a list of all pairs of numbers between 0 and 20 such that their sum is 30
-- [(10,20) ,(11,19), (12,18), (13,17), (14,16), (15,15), (16,14), (17,13), (18,12), (19,11),(20,10)]
-- unlike the previous case, making a function is more complicating than strapping the definition on a filter
-- this because we are handling concurrent programming

-- [sum [y | y <- [1..x], odd y] | x <-[1..10]]
