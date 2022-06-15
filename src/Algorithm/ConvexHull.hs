module Algorithm.ConvexHull where 

import Data.Geometry.Point
import Data.Geometry.Orientation

grahamScan :: (Num a, Ord a) => [Point2D a] -> [Point2D a]
grahamScan []     = []
grahamScan [x]    = [x]
grahamScan [x, y] = [x, y]
grahamScan xs     = undefined