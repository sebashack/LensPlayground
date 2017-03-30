{-# LANGUAGE TemplateHaskell, RankNTypes #-}

module Lib where


import Control.Lens
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Identity
import Data.Monoid
import Data.Functor.Contravariant


data Point = Point {
    _positionX :: Double
  , _positionY :: Double
  } deriving (Show)
makeLenses ''Point

data Segment = Segment {
    _segmentStart :: Point
  , _segmentEnd :: Point
  } deriving (Show)
makeLenses ''Segment


makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

makeSegment :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)


-- << Traversals
type Traversal_ s t a b = forall f.
  Applicative f => (a -> f b) -> s -> f t

-- pointCoordinates :: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates :: Traversal_ Point Point Double Double -- Generalized Type
pointCoordinates g (Point x y) = Point <$> g x <*> g y

myPositionX :: Traversal_ Point Point Double Double
myPositionX g (Point x y) = Point <$> g x <*> pure y

myPositionY :: Traversal_ Point Point Double Double
myPositionY g (Point x y) = Point <$> pure x <*> g y

-- extremityCoordinates :: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoordinates :: Traversal Segment Segment Double Double
extremityCoordinates g (Segment start end)
  = Segment <$> pointCoordinates g start <*> pointCoordinates g end

mySegmentStart :: Traversal Segment Segment Double Double
mySegmentStart g (Segment start end) = Segment <$> pointCoordinates g start <*> pure end

mySegmentEnd :: Traversal Segment Segment Double Double
mySegmentEnd g (Segment start end) = Segment <$> pure start <*> pointCoordinates g end

deleteIfNegative x
  | x < 0 = Nothing
  | otherwise = Just x

travPoint1 = pointCoordinates deleteIfNegative (makePoint (1, 2))
travPoint2 = pointCoordinates deleteIfNegative (makePoint (-1,2))

travSegment1 = extremityCoordinates deleteIfNegative (makeSegment (1,2) (1,3))
travSegment2 = extremityCoordinates deleteIfNegative (makeSegment (1,-2) (1,3))


traverse' :: Traversable t => Traversal (t a) (t b) a b
traverse' = traverse
-- >>


-- << Setters
type Setter_ s t a b = (a -> Identity b) -> s -> Identity t

over_ :: Setter s t a b -> (a -> b) -> s -> t
over_ f g s = runIdentity $ f g' s
  where
    g' = (Identity . g)

set_ :: Setter_ s t a b ->  b -> s -> t
set_ f v s = over f (const v) s

scaleSegment :: Double -> Segment -> Segment
scaleSegment n seg = over_ extremityCoordinates (n*) seg
-- >>

-- << Folds
type Getting_ r s a = (a -> Const r a) -> s -> Const r s

type Fold_ s a = forall m .
  Monoid m => Getting_ m s a

type Fold__ s a = forall f .
  (Contravariant f, Applicative f) => (a -> f a) -> s -> f s

foldMapOf_ :: Getting_ r s a -> (a -> r) -> s -> r
foldMapOf_ l f s = getConst (g s)
  where
    g = l (Const . f)

toListOf_ :: Getting_ [a] s a -> s -> [a]
toListOf_ l = foldMapOf_ l (:[])

-- pointGetting :: Monoid m => Getting_ m Point Double
pointGetting :: Fold_ Point Double
pointGetting g (Point x y) = Const (getConst $ g (x + y))


-- segmentGetting :: Monoid m => Getting_ m Segment Double
segmentGetting :: Fold_ Segment Double
segmentGetting g (Segment start end) =
  Const $ getConst $ g $ (getSum $ foldPoint start) + (getSum $ foldPoint end)
  where
    foldPoint = foldMapOf pointGetting Sum


fold1 = foldMapOf pointGetting Sum (makePoint (1,2))
fold2 = foldMapOf segmentGetting Sum (makeSegment (1,2) (2,3))

pointList1 = toListOf pointCoordinates (makePoint (1,2))
pointList2 = toListOf extremityCoordinates (makeSegment (1,2) (2,3))
-- >>


myMapped :: Setter (Identity a) (Identity b) a b
myMapped g (Identity a) = Identity <$> g a


-- << Examples
set1 = over_ pointCoordinates negate (makePoint (1, 2))
set2 = set_ pointCoordinates 7 (makePoint (1, 2))

-- << Now our version of lenses
setx = set myPositionX 10 (makePoint (1, 2))
sety = set myPositionY 15 (makePoint (1, 2))

setStart = over mySegmentStart (*10) (makeSegment (1, 2) (3, 4))
setEnd = over mySegmentEnd (*10) (makeSegment (1, 2) (3, 4))

mapIt = over myMapped (*10) (Identity 1)

scaled1 = scaleSegment 8 (makeSegment (1,1) (2,2))
