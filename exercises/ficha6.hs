module BinaryTrees where

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

height :: BTree a -> Int
height Empty = 0
height (Node n l r) = 1 

countNodes :: BTree a -> Int
countNodes Empty = 0
countNodes (Node n l r) = (+) 1 ((+) (countNodes l) (countNodes r))

leafNodes :: BTree a -> Int
leafNodes Empty = 0
leafNodes (Node n Empty Empty) = 1
leafNodes (Node n l r) = (+) (leafNodes l) (leafNodes r)

prune :: Int -> BTree a -> BTree a
prune 0 _ = Empty
prune 1 (Node n _ _) = Node n Empty Empty
prune x (Node n l r) = Node n (prune (x-1) l) (prune (x-1) r)

path :: [Bool] -> BTree a -> [a]
path [] _                = []
path _ Empty = []
path (x:xs) (Node n l r) = if x == False then n:(path xs l) else n:(path xs r)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node n l r) = Node n (mirror r) (mirror l)  

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node n l r) (Node p lt rt) = Node (f n p) (zipWithBT f l lt) (zipWithBT f r rt)
zipWithBT _ _ _ = Empty

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty,Empty) 
unzipBT (Node (na,nb,nc) l r) = (Node na la ra, Node nb lb rb, Node nc lc rc)
                 where
                    (la,lb,lc) = unzipBT l
                    (ra,rb,rc) = unzipBT r

-- BST
minimumBS :: Ord a => BTree a -> a
minimumBS Empty = error "Cannot be an empty tree"
minimumBS (Node n Empty _) = n
minimumBS (Node n l _) = minimumBS l

noMinimum :: Ord a => BTree a -> BTree a
noMinimum Empty = error"Cannot be an empty tree"
noMinimum (Node _ Empty _) = Empty
noMinimum (Node n l r) = Node n (noMinimum l) r

minWmin :: Ord a => BTree a -> (a,BTree a)
minWmin Empty = error"Cannot be an empty tree"
minWmin (Node n Empty _) = (n, Empty)
minWmin (Node n l r) = (m, Node n ln r)
                      where
                          (m, ln) = minWmin l

--remove :: Ord a => a -> BTree a -> BTree a
--remove Empty = error"No empty trees"

-- 3
type Student = (Number, Name, Type, Grade)
type Number = Int
type Name = String
data Type = STD | SW | IMP deriving Show
data Grade = Aprov Int
       | Rep
       | Missed
   deriving Show
type Class = BTree Student -- Binary Search Tree (ordered by number)

inscNumCheck :: Number -> Class -> Bool
inscNumCheck _ Empty = False
inscNumCheck x (Node (n,_,_,_) l r) = x == n || inscNumCheck x (if x > n then r else l)


inscNameCheck :: Name -> Class -> Bool
inscNameCheck _ Empty = False
inscNameCheck x (Node(_,n,_,_) l r) = x == n || (inscNameCheck x l || inscNameCheck x r)

studWork :: Class -> [(Number, Name)]
studWork Empty = []
studWork (Node (n,name,t,_) l r) = (case t of SW -> [(n,name)];otherwise -> []) ++ studWork l ++ studWork r

grade :: Number -> Class -> Maybe Grade -- if not in class then Nothing
grade _ Empty = Nothing
grade x (Node (n,_,_,g) l r) = if x == n then Just g else (if x > n then grade x r else grade x l) 


percMissed :: Class -> Float
percMissed Empty = 0
percMissed (Node n l r) = (countMissed (Node n l r)/countStud (Node n l r)) *100
                where
                   countMissed Empty = 0
                   countMissed (Node (_,_,_,g) l r) = (case g of Missed -> 1;_ -> 0) + countMissed l + countMissed r
                   countStud Empty = 0
                   countStud (Node _ l r) = 1 + countStud l + countStud r

--avrgAprov :: Class -> Float
--avrgAprov Empty = 0
--avrgAprov (Node n l r) 

--AprovPerGraded :: Class -> Float -- ratio of students that passed by graded studens only one passing of the tree

