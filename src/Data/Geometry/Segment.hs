module Data.Geometry.Segment where 

import Data.Geometry.Point

data Segment2D a = Segment2D (Point2D a) (Point2D a) deriving (Eq, Read, Show)
