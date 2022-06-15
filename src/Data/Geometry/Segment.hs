module Data.Geometry.Segment where 

import Data.Geometry.Point

data Segment2D a = Segment2D (Point2D a) (Point2D a) deriving (Eq, Read, Show)

instance Functor Segment2D where 
    fmap f (Segment2D p q) = Segment2D (f <$> p) (f <$> q)