module Data.Geometry.Orientation where 

import Data.Geometry.Point

data Orientation = Collinear | Counterclockwise | Clockwise deriving (Eq, Show)

orientation :: (Num a, Ord a, Eq a) => Point2D a -> Point2D a -> Point2D a -> Orientation 
orientation p q r 
    | cp == 0   = Collinear 
    | cp >  0   = Counterclockwise 
    | otherwise = Clockwise
    where cp = crossProduct p q r