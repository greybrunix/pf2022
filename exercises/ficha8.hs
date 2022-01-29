module TypeClasses where

data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc a 0 = a
mdc a b = mdc b (mod a b)
normalize :: Frac -> Frac
normalize (F a b) = F (div a c) (div b c)
        where c = mdc a b

instance Eq Frac where
  (F a b) == (F c d) = compareAux (normalize (F a b)) (normalize (F c d))
                    where compareAux (F a b) (F c d) = a*d == b*c

instance Ord Frac where
  compare (F a b) (F c d)
            | c1 < c2 = LT
            | c1 == c2 = EQ
            | c1 > c2 = GT
              where
                c1 = (fromIntegral a)/(fromIntegral b)
                c2 = (fromIntegral c)/(fromIntegral d)

instance Show Frac where
  show (F a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

instance Num Frac where
  (+) (F a b) (F c d) = (F (a*d + b*c) (b*d))
  (*) (F a b) (F c d) = (F (a*c) (b*d))
  (-) (F a b) (F c d) = (F (a*d - b*c) (b*d))
  negate (F a b) = (F ((-) 0 a) b)
  abs (F a b) = (F (abs (a)) (abs (b)))
  signum (F a b) = normalize (F (signum a) (signum b))
  fromInteger x = (F x 1) 

selFrac :: Frac -> [Frac] -> [Frac]
selFrac _ [] = []
selFrac f l = filter (\x -> f * fromIntegral 2 < x) l

data Exp a = Const a
           | Symmetric (Exp a)
           | Sum (Exp a) (Exp a)
           | Sub (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
  show (Const a) = show a
  show (Symmetric a) = "- " ++ show a
  show (Sum n m) = show n ++ " + " ++ show m
  show (Sub n m) = show n ++ " - " ++ show m
  show (Mult n m) = show n ++ " * " ++ show m

calculate :: Num a => Exp a-> Int
calculate (Const x) = x
calculate (Symmetric x) = (- calculate x)
calculate (Sum a b) = (+) (calculate a) (calculate b)
calculate (Sub a b) = (-) (calculate a) (calculate b)
calculate (Mult a b) = (*) (calculate a) (calculate b)

instance (Num a,Eq a) => Eq (Exp a) where
  (==) n m  = (==) (calculate n) (calculate m)


instance (Num a, Eq a) => Num (Exp a) where
  x + y = Const ((calculate x) + (calculate y))
  x - y = Const ((calculate x) - (calculate y))
  x * y = Const ((calculate x) * (calculate y))
  negate (Const a) = Const (- a)
  negate (Symmetric a) = a
  negate (Sum a b) = Sum (- a) (- b)
  negate (Sub a b) = Sub b a
  negate (Mult a b) = Mult (-a) b
  abs (Const a) = Const (abs a)
  abs (Symmetric a) = abs a
  abs (Sum a b) = abs (a + b)
  abs (Sub a b) = abs (a - b)
  abs (Mult a b) = abs (a * b)
  signum (Const a) = Const (if abs a == a then if a == 0 then 0 else 1 else (-1))
  signum (Symmetric a) = - signum a
  signum (Sum a b) = Const (if abs (a + b) == a + b then if a + b == 0 then 0 else 1 else (-1))
  signum (Sub a b) = Const (if abs (a - b) == a - b then if a - b == 0 then 0 else 1 else (-1))
  signum (Mult a b) = Const (if abs (a * b) == a * b then if a * b == 0 then 0 else 1 else (-1))
  fromInteger x = (Const (fromInteger x))

data Transfer = Credit Float | Debt Float
data Data = D Int Int Int deriving Eq
data Extract = Ext Float [(Data, String, Transfer)]

