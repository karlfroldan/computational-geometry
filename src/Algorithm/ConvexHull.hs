module Algorithm.ConvexHull where 

import Data.Geometry.Point
import Data.Geometry.Orientation

import Data.List (sort)

import Debug.Trace (trace)

grahamScan :: (Num a, Ord a, Show a) => [Point2D a] -> [Point2D a]
grahamScan []     = []
grahamScan [x]    = [x]
grahamScan [x, y] = [x, y]
grahamScan xs     = upperhull ++ (tail . init) lowerhull
-- grahamScan xs     = upperhull ++ (tail . init) lowerhull
    where upperhull = hull (drop 3 xs') ((reverse . take 3) xs')
          lowerhull = hull (drop 3 xs'')  ((reverse . take 3) xs'')
          xs'       = sort xs
          xs''      = reverse xs'

{-
Assumptions:

The current hull will always have at least 2 points in it.
-}
hull :: (Num a, Ord a, Show a) 
          => [Point2D a] -- List of unmet points
          -> [Point2D a] -- Current hull
          -> [Point2D a]
--hull [] ps = ps -- if there are no points remaining
hull (s:ps) [p, q] = hull ps [s, p, q]
hull (s:ps) (r:q:p:currentHull)
    | makesRightTurn p q r = hull ps (s:r:q:p:currentHull)
    -- If they make a left turn, delete the middle (in this case, q)
    | otherwise            = hull (s:ps) (r:p:currentHull)
hull [] (r:q:p:currentHull) 
    | makesRightTurn p q r =  r:q:p:currentHull
    | otherwise            =  r:p:currentHull
hull _ _ = undefined

makesRightTurn :: (Num a, Ord a) =>  Point2D a -> Point2D a -> Point2D a -> Bool 
makesRightTurn p q r = orientation p q r == Clockwise

showPoints :: Show a => a -> a -> a -> String 
showPoints p q r = concat 
    [ "p: ", show p
    , ", q: ", show q
    , ", r: ", show r ]