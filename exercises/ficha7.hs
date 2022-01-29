module OtherTrees where

data ExpInt = Const Int
            | Symmetric ExpInt
            | Sum ExpInt ExpInt
            | Sub ExpInt ExpInt
            | Mult ExpInt ExpInt

calculate :: ExpInt -> Int
calculate (Const x) = x
calculate (Symmetric x) = (- calculate x)
calculate (Sum a b) = (+) (calculate a) (calculate b)
calculate (Sub a b) = (-) (calculate a) (calculate b)
calculate (Mult a b) = (*) (calculate a) (calculate b)

infixEI :: ExpInt -> String
infixEI (Const x) = show x
infixEI (Symmetric x) = "(-" ++ (infixEI x) ++ ")"
infixEI (Sum a b) = "(" ++ (infixEI a) ++ " + "  ++ (infixEI b) ++ ")"
infixEI (Sub a b) = "(" ++ (infixEI a) ++ " - " ++ (infixEI b) ++ ")"
infixEI (Mult a b) = "(" ++ (infixEI a) ++ " * "  ++ (infixEI b) ++ ")"

posfixEI :: ExpInt -> String
posfixEI (Const x) = show x ++ " "
posfixEI (Symmetric x) = (posfixEI x) ++ "- "
posfixEI (Sum a b) = (posfixEI a) ++ (posfixEI b) ++ "+ "
posfixEI (Sub a b) = (posfixEI a) ++ (posfixEI b) ++ "- "
posfixEI (Mult a b) = (posfixEI a) ++(posfixEI b) ++ "* "

data RTree a = R a [RTree a] deriving Show

sumRT :: Num a => RTree a -> a
sumRT (R x []) = x
sumRT (R x xs) = x + sum (map sumRT xs)

heightRT :: RTree a -> Int
heightRT (R _ []) = 1
heightRT (R n xs) = 1 + maximum (map heightRT xs)

pruneRT :: Int -> (RTree a -> RTree a)
pruneRT 0 (R x xs) = R x []
pruneRT n (R x xs) = R x (map (pruneRT (n-1)) xs) 

mirror :: RTree a -> RTree a
mirror (R n xs) = R n (map mirror (reverse xs))

postorder :: RTree a -> [a]
postorder (R x []) = [x]
postOrder (R x xs) = concatMap postorder xs ++ [x]

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
-- Tree in which information is in the nodes whereas the leaves are empty

data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show
-- leaf tree
-- all information is in the leaves


ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork a b) = ltSum a + ltSum b

listLT :: LTree a -> [a]
listLT (Tip x) = x:[]
listLT (Fork a b) = listLT a ++ listLT b

ltHeight :: LTree a -> Int
ltHeight (Tip _) = 1
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)

data FTree a b = Leaf b | Vertex a (FTree a b) (FTree a b) deriving Show
-- this is a full tree which combines the approach of the binary tree and the leaft tree by having information in both nodes and leaves, -> all nodes contain information

splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (Vertex n l r) = (Node n (fst (splitFTree l)) (fst (splitFTree r)) , Fork (snd (splitFTree l)) (snd (splitFTree r)))

joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node n l r) (Fork a b) = case joinTrees l a of
                      Nothing -> Nothing
                      Just ln -> case joinTrees r b of
                                 Just rn -> Just (Vertex n ln rn)
                                 Nothing -> Nothing
joinTrees _ _ = Nothing

-- This function does not work as expected in the exception case of not being compatible
