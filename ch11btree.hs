module Chapter11BTree where


data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insertBT :: Ord a => a -> BinaryTree a -> BinaryTree a
insertBT e Leaf = Node Leaf e Leaf
insertBT e (Node l x r)
  | e > x     = Node l x (insertBT e r)
  | e < x     = Node (insertBT e l) x r
  | otherwise = Node l x r

mapBT :: (a -> b) -> BinaryTree a -> BinaryTree b
mapBT _ Leaf         = Leaf
mapBT f (Node l x r) = Node (mapBT f l) (f x) (mapBT f r)

toListBT :: BinaryTree a -> [a]
toListBT Leaf         = []
toListBT (Node l x r) = x : toListBT l ++ toListBT r

foldBT :: (a -> b -> b) -> b -> BinaryTree a -> b
foldBT _ z Leaf         = z
foldBT f z (Node l x r) = f x (foldBT f (foldBT f z r) l)
