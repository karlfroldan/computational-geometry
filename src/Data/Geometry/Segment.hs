module Data.Geometry.Segment where 

import Data.Geometry.Point

data Segment2D a = Segment2D (Point2D a) (Point2D a) deriving (Eq, Read, Show)

instance Functor Segment2D where 
    fmap f (Segment2D p q) = Segment2D (f <$> p) (f <$> q)

xIntercept :: (Fractional k, Ord k) => k -> Segment2D k -> k 
xIntercept y s = (y - b) / m
    where m = slope s
          b = bias s

yIntercept :: (Fractional k, Ord k) => k -> Segment2D k -> k 
yIntercept x s = (m * x) + b 
    where m = slope s 
          b = bias s

-- | Calculate the slope of a segment
slope :: Fractional a => Segment2D a -> a 
slope (Segment2D p1 p2) = (y2 - y1) / (x2 - x1) 
    where Point2D x1 y1 = p1 
          Point2D x2 y2 = p2

-- | Calculate the bias of a given segment
bias :: Fractional a => Segment2D a -> a 
bias s@(Segment2D p1 _) = y - (m * x)
    where m           = slope s 
          Point2D x y = p1 