{-# LANGUAGE RankNTypes  #-}

module Traversal where

import Segment


-- | Traversal Type
type Traversal s t a b = forall f .
  Applicative f => (a -> f b) -> s -> f t


-- | Traversable and Traversal relationship:
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' :: Traversable t => Traversal (t a) (t b) a b
traverse' = traverse

-- | Examples: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates :: Traversal Point Point Double Double
pointCoordinates g (Point x y) = Point <$> g x <*> g y

positionX :: Traversal Point Point Double Double
positionX g (Point x y) = Point <$> g x <*> pure y

positionY :: Traversal Point Point Double Double
positionY g (Point x y) = Point <$> pure x <*> g y

-- | Examples: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoordinates :: Traversal Segment Segment Double Double
extremityCoordinates g (Segment start end)
  = Segment <$> pointCoordinates g start <*> pointCoordinates g end

segmentStart :: Traversal Segment Segment Double Double
segmentStart g (Segment start end) = Segment <$> pointCoordinates g start <*> pure end

segmentEnd :: Traversal Segment Segment Double Double
segmentEnd g (Segment start end) = Segment <$> pure start <*> pointCoordinates g end
