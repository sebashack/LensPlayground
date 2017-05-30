module Segment where

data Point = Point {
    _positionX :: Double
  , _positionY :: Double
  } deriving (Show)


data Segment = Segment {
    _segmentStart :: Point
  , _segmentEnd :: Point
  } deriving (Show)

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

unmakePoint :: Point -> (Double, Double)
unmakePoint (Point x y) = (x,y)

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)
