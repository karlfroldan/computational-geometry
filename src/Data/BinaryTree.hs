module Data.BinaryTree where

import Data.Foldable (Foldable(foldl'))
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)

{-
RATIONALE:

Data.Map does not provide a function where we can insert a dynamic key. What exactly do we mean by this?

Suppose we have some key `k` we want to insert into a tree `T`. However, the ordering of the tree is dynamic,
provided that we can prove that it still maintains the red-black tree property throughout the lifetime of the data structure.
That is, when we insert `k` into `T`, we apply an ordering function `f` to `k` but inserts `k` instead of `f k` to `T`.

For now, I just want a working binary search tree. I will convert the entire file (without changing the API) into a 
balanced binary search tree later on, perhaps a red-black tree.
-}
data Color = Black | Red deriving (Eq, Show)
data BinTree k v = Leaf | Node (BinTree k v) k v (BinTree k v) deriving (Eq, Show)

-- | Insert into the binary tree with key `k` but with `f` applied.
insertWith :: Ord k => (k -> k) -> k -> v -> BinTree k v -> BinTree k v
insertWith f k v Leaf = Node Leaf k v Leaf
insertWith f k v n@(Node l k' v' r)
    | f k < f k' = Node (insertWith f k v l) k' v' r
    | f k > f k' = Node l k' v' (insertWith f k v r)
    | otherwise  = n -- do nothing.

-- | The same insert function as `Data.Map`.
insert :: Ord k => k -> v -> BinTree k v -> BinTree k v
insert = insertWith id

minView :: BinTree k v -> BinTree k v
minView Leaf                = Leaf
minView n@(Node Leaf k v r) = n
minView (Node l k v r)      = minView l

maxView :: BinTree k v -> BinTree k v
maxView Leaf                = Leaf
maxView n@(Node r k v Leaf) = n
maxView (Node l k v r)      = maxView r

--delete :: (Eq k, Ord k) => k -> BinTree k v -> BinTree k v 

delete :: (Eq k, Ord k) => k -> BinTree k v -> BinTree k v
delete k Leaf = Leaf
delete k (Node Leaf k' v r) -- = Node Leaf k' v (delete k r)
    | k == k'   = r
    | otherwise = Node Leaf k' v (delete k r)
delete k (Node l k' v Leaf) -- = Node (delete k l) k' v Leaf
    | k == k'   = l
    | otherwise = Node (delete k l) k' v Leaf
delete k (Node l k' v r)
    | k < k' = Node (delete k l) k' v r
    | k > k' = Node l k' v (delete k r)
    | otherwise  = Node l k'' v' r'
    -- swap this with the leftmost child of the right subtree and delete that.
    where (k'', v', r') = deleteMin r -- delete the minimum of the right child.

deleteMin :: BinTree k v -> (k, v, BinTree k v)
deleteMin Leaf = error "Cannot delete the minimum from an empty tree"
deleteMin (Node Leaf k v r) = (k, v, r)
deleteMin (Node l k v r)    = (k', v', Node tree k v r)
    where (k', v', tree) = deleteMin l

deleteMax :: BinTree k v -> (k, v, BinTree k v)
deleteMax Leaf = error "Cannot delete the maximum from an empty tree"
deleteMax (Node l k v Leaf) = (k, v, l)
deleteMax (Node l k v r)    = (k', v', Node l k v tree)
    where (k', v', tree) = deleteMax r

fst3 :: (a, b, c) -> a
fst3 (x, y, z) = x
snd3 :: (a, b, c) -> b
snd3 (x, y, z) = y
trd3 :: (a, b, c) -> c
trd3 (x, y, z) = z

fromListWith :: Ord k => (k -> k) -> [(k, v)] -> BinTree k v
fromListWith f = foldl' (\acc (k, v) -> insertWith f k v acc) Leaf

fromList :: Ord k => [(k, v)] -> BinTree k v
fromList = fromListWith id

toList :: BinTree k v -> [(k, v)]
toList Leaf = []
toList (Node l k v r) = toList l ++ [(k, v)] ++ toList r

-- A Binary tree is a functor.
instance Functor (BinTree a) where
    fmap f Leaf           = Leaf
    fmap f (Node l k v r) = Node (fmap f l) k (f v) (fmap f r)

instance Foldable (BinTree a) where
    foldMap f Leaf           = mempty
    foldMap f (Node l k v r) = foldMap f l `mappend` f v `mappend` foldMap f r

instance Ord k => Semigroup (BinTree k v) where
    (<>) = union

instance Ord k => Monoid (BinTree k v) where
    mempty  = Leaf
    mappend = (<>)

unionWith :: Ord k => (k -> k) -> BinTree k v -> BinTree k v -> BinTree k v
unionWith _ tree Leaf = tree
unionWith f tree (Node l k v r) = unionWith f (insertWith f k v tree) r

union :: Ord k => BinTree k v -> BinTree k v -> BinTree k v
union = unionWith id

showTree :: (Show k, Show v) => BinTree k v -> IO ()
showTree = showTree' 0

showTree' :: (Show k, Show v) => Int -> BinTree k v -> IO ()
showTree' i Leaf = putStr (replicate (i * 2) ' ' ++ "> LEAF\n")
showTree' i (Node l k v r) = do
    putStr (replicate (i * 2) ' ' ++ "> " ++ show k ++ " | " ++ show v ++ "\n")
    showTree' (i + 1) l
    showTree' (i + 1) r

xs = fromList [(0, 1), (-1, 4), (8, 0), (-100, 40), (40, -50), (-50, 60), (60, 80), (45, 43), (21, 90)]

{-
FUNCTIONS FOR TRAVERSING THE BINARY SEARCH TREE

These functions are important so that we can easily find the successor and predecessor of 
nodes efficiently. Otherwise, we would have to convert the tree to a list every time which might
not be efficient for large-enough trees.
-}

search :: Ord k => k -> BinTree k v -> Maybe (BinTree k v)
search k Leaf = Nothing 
search k n@(Node l k' v r)
    | k < k' = search k l 
    | k > k' = search k r 
    | otherwise = Just n

value :: BinTree k v -> v 
value Leaf           = error "Data.BinaryTree cannot get value of a leaf. Make sure tree is nonempty"
value (Node _ _ v _) = v