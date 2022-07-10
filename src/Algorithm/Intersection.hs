module Algorithm.Intersection where 

import qualified Data.Map.Lazy as M
import qualified Data.Set      as S 

import Data.Map  (Map)
import Data.Set  (Set)
import Data.List (sortBy, foldl')

import Data.Geometry.Point
import Data.Geometry.Segment

-- | Contains the slope and b of a line.
-- data LineKey a = LineKey a a deriving (Eq, Show)
-- data LineKey a = LineKey
--     { slope :: Fractional a => Segment2D a -> a 
--     , bias :: Fractional a => Segment2D a -> a 
--     , getY :: Fractional a => }

(>--) :: Ord a => Point2D a -> Point2D a -> Ordering 
Point2D x1 y1 >-- Point2D x2 y2
    | y1 == y2  = x1 `compare` x2 
    | otherwise = y2 `compare` y1

bentleyOttman :: [Segment2D a] -> Map (Point2D a) (Segment2D a)
bentleyOttman sgmts = undefined 