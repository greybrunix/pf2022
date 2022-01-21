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

remove :: Ord a => a -> BTree a -> BTree a
remove Empty = error"No empty trees"