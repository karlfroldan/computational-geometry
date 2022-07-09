module Main where 

import Data.Geometry.Point (Point2D, point)
import Data.Ratio (Rational, (%))
import Algorithm.ConvexHull (grahamScan)

main :: IO ()
main = print $ grahamScan points

points :: [Point2D Rational]
points = point <$> lst 
    where 
        lst = [ (4 % 5, 1 % 4)
              , (10 % 4, 2 % 1)
              , (100 % 93, -93 % 64)
              , (-121 % 41, 65 % 3)
              , (-200 % 11, 3 % 4)
              , (11 % 13, -13 % 53)
              , (35 % 4, -4 % 5)
              , (-15 % 1, 15 % 1)
              , (0 % 1, -15 % 1)
              , (10, 18)
              , (5, -10)
              ]
-- \operatorname{polygon}\left(\left(-11,14\right),\left(0,\ 13\right),\left(14,7\right),\left(15,-6\right),\ \left(9,-11\right),\ \left(-3,-6\right)\right)
points2 :: [Point2D Double]
points2 = point <$> lst 
    where 
        lst = [ (4, 5), (-4, 5), (6, 7), (0, 13), (1, -2), (0, 1), (-11, 14)
              , (-3, -6), (8, -9), (9, -11), (15, -6), (14, 7)]

-- (-11, 14), (0, 13), (14, 7), (15, -6), (9, 11), (-3, -6)