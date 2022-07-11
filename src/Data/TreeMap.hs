module Data.TreeMap where 

import Data.BinaryTree (BinTree (..))
import qualified Data.BinaryTree  as BT

{-
A data structure to encapsulate the details of the `BinTree k v` implemented in
`Data.BinaryTree`. 
-}

newtype TreeMap k v = TreeMap {
    getTree :: BinTree k v
    } deriving Eq

instance Functor (TreeMap k) where 
    fmap f t = TreeMap (fmap f (getTree t))

instance Ord k => Semigroup (TreeMap k v) where 
    t <> g = TreeMap (getTree t <> getTree g)

instance Ord k => Monoid (TreeMap k v) where 
    mempty = TreeMap Leaf 
    mappend = (<>)

instance Foldable (TreeMap k) where 
    foldMap f t = case getTree t of 
      Leaf             -> mempty
      n@(Node l k v r) -> foldMap f n

-- instance (Show k, Show v) => Show (TreeMap k v) where 
--     show t = BT.showTree (getTree t)

fromListWith :: Ord k => (k -> k) -> [(k, v)] -> TreeMap k v 
fromListWith f = TreeMap . BT.fromListWith f 

fromList :: Ord k => [(k, v)] -> TreeMap k v 
fromList = fromListWith id

insertWith :: Ord k => (k -> k) -> k -> v -> TreeMap k v -> TreeMap k v 
insertWith f k v t = TreeMap (BT.insertWith f k v (getTree t))

insert :: Ord k => k -> v -> TreeMap k v -> TreeMap k v 
insert = insertWith id 

minView :: TreeMap k v -> Maybe (k, v)
minView t = case BT.minView (getTree t) of
    Leaf -> Nothing
    n    -> Just (BT.key n, BT.value n)

maxView :: TreeMap k v -> Maybe (k, v)
maxView t = case BT.maxView (getTree t) of
    Leaf -> Nothing
    n    -> Just (BT.key n, BT.value n)

delete :: (Eq k, Ord k) => k -> TreeMap k v -> TreeMap k v 
delete k t = TreeMap (BT.delete k (getTree t))

deleteMin :: TreeMap k v -> (k, v, TreeMap k v)
deleteMin t = apply3 TreeMap (BT.deleteMin (getTree t))

deleteMax :: TreeMap k v -> (k, v, TreeMap k v)
deleteMax t = apply3 TreeMap (BT.deleteMax (getTree t))

apply3 :: (t -> c) -> (a, b, t) -> (a, b, c)
apply3 f (x, y, z) = (x, y, f z)