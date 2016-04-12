module Hasklet1 where


-- | A generic binary tree with values at internal nodes.
data Tree a = Node a (Tree a) (Tree a)
            | Leaf
  deriving (Eq,Show)

-- Some example trees.
t1, t2, t3 :: Tree Int
t1 = Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf)
t2 = Node 4 t1 (Node 5 Leaf (Node 6 Leaf Leaf))
t3 = Node 7 t1 t2


-- | Sum the integers in a binary tree.
--
--   >>> tsum Leaf
--   0
--
--   >>> tsum t1
--   6
--
--   >>> tsum t2
--   21
--
--   >>> tsum t3
--   34
--
tsum :: Tree Int -> Int
tsum Leaf = 0
tsum (Node a b c) = a + tsum b + tsum c


-- | Check if a given element is contained in a tree.
--
--   >>> contains 5 t1
--   False
--
--   >>> contains 5 t2
--   True
--
--   >>> contains 5 t3
--   True
--
contains :: Eq a => a -> Tree a -> Bool
contains _ Leaf = False
contains x (Node a b c) = if x == a
							then True
							else contains x b || contains x c



-- | Fold over a binary tree.
--tfold :: (a -> b -> b) -> b -> (Tree a) -> b
--tfold f b Leaf = b
--tfold f b (Node x y z) = tfold f (f x (tfold f b y)) z

tfold :: (a -> b -> b -> b) -> b -> (Tree a) -> b
tfold _ b Leaf = b
tfold f b (Node x y z) = f x (tfold f b y) (tfold f b z)


-- mechanically derived fold over more complicated data type.
--data Tree a b = Leaf
--			  | Red a (Tree a b)
--			  | Blue b (Tree a b) (Tree a b)

--tfold :: (a -> c -> c) -> (b -> c -> c -> c) -> c -> Tree a b -> c
--tfold f g c Leaf = c 
--tfold f g c (Red a t) = f a (tfold f g c t)
--tfold f g c (Blue b l r) = g b (tfold f g c l) (tfold f g c r)


-- | Sum the integers in a binary tree, this time using tfold.
--
--   >>> tsum' Leaf
--   0
--
--   >>> tsum' t1
--   6
--
--   >>> tsum' t2
--   21
--
--   >>> tsum' t3
--   34
--   
tsum' :: Tree Int -> Int
tsum' Leaf = tfold (+) 0 Leaf 
tsum' a = tfold (+) 0 a


-- | Check if a given element is contained in a tree, this time using tfold.
--
--   >>> contains' 5 t1
--   False
--
--   >>> contains' 5 t2
--   True
--
--   >>> contains' 5 t3
--   True
--

contains' :: Eq a => a -> Tree a -> Bool
contains' a Leaf = tfold (==) False Leaf 
contains' a (Node x y z) = if x == a
							then True
							else contains' a y || contains' a z


-- | Return a nested list where the first list contains the value at the root,
--   the second list contains the values at its children, the third list
--   contains the values at the next level down the tree, and so on.
--
--   >>> levels Leaf
--   []
--   
--   >>> levels t1
--   [[1],[2,3]]
--
--   >>> levels t2
--   [[4],[1,5],[2,3,6]]
--
--   >>> levels t3
--   [[7],[1,4],[2,3,1,5],[2,3,6]]
--   

levels :: Tree a -> [[a]]
levels Leaf = tfold (:) [] Leaf
levels (Node x y z) = [x] : zipWith (++) (levels y) (levels z)


--levels (Node x y z) = [x]: foldr (zipWith' (++)) [] (map levels y levels z)
--levels (Node x y z) = [x]: (zipWith' (++)  y z)


--zipWith' :: (a -> a -> b) -> Tree a -> Tree a -> [b]
--zipWith' _ Leaf Leaf = []
--zipWith' _ Leaf a = a
--zipWith' _ a Leaf = a  
--zipWith' f (Node a b c) (Node x y z) = f a x : zipWith' f b c : zipWith' f c z
						
--zipWith' :: (a -> a -> b) -> Tree a -> Tree a -> [b]
--zipWith' f xs [] = xs
--zipWith' f [] xs = xs
--zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
