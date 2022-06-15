module Data.Geometry.Point where 

data Point2D a = Point2D a a deriving (Eq, Read)

-- | Construct a point given a tuple.
point :: (a, a) -> Point2D a
point = uncurry Point2D

crossProduct :: Num a => Point2D a -> Point2D a -> Point2D a -> a 
crossProduct (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = 
    (x2 - x1) * (y3 - y1) - (x3 - x1) * (y2 - y1)

-- a point is a functor. But it is not an applicative, I think. haha
instance Functor Point2D where 
    fmap f (Point2D x y) = Point2D (f x) (f y)

instance Show a => Show (Point2D a) where 
    show (Point2D x y) = 
        mconcat [ "(", show x , ", ", show y, ")"]