{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}

module Lib where


import Control.Lens
import Control.Applicative
import Data.Functor.Identity
import Control.Monad.Identity
import Data.Monoid
import Data.Functor.Contravariant
import Control.Monad.Reader


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

-- pointGetting :: Getting_ (Sum Double) Point Double
pointGetting :: Fold_ Point Double
pointGetting g (Point x y) = Const (getConst $ g (x + y))

-- segmentGetting :: Monoid m => Getting_ m Segment Double
segmentGetting :: Fold_ Segment Double
segmentGetting g (Segment start end) =
  Const $ getConst $ g $ getSum $ (foldPoint start) <> (foldPoint end)
  where
    foldPoint = foldMapOf_ pointGetting Sum


fold1 = foldMapOf pointGetting Sum (makePoint (1,2))
fold2 = foldMapOf segmentGetting Sum (makeSegment (1,2) (2,3))

pointList1 = toListOf pointCoordinates (makePoint (1,2))
pointList2 = toListOf extremityCoordinates (makeSegment (1,2) (2,3))
-- >>


myMapped :: Setter (Identity a) (Identity b) a b
myMapped g (Identity a) = Identity <$> g a


-- << Getters
type Getter_ s a = forall r . Getting_ r s a

type Getter__ s a = forall f .
  (Contravariant f, Functor f) => (a -> f a) -> s -> f s


---- asks --------
ask_ :: Monad m => ReaderT a m a
ask_ = ReaderT (\a -> return a)

asks_ :: Monad m => (r -> a) -> ReaderT r m a
asks_ f = ReaderT (\r -> return $ f r)
------------------

view_ :: Monad m => Getting_ a s a -> ReaderT s m a
view_ l = asks_ $ getConst . (l Const)

getterPointX :: Getting Double Point Double
getterPointX f (Point x y) = Const $ getConst $ f x

getterPointY :: Getting Double Point Double
getterPointY f (Point x y) = Const $ getConst $ f y

coordX :: Double
coordX = runIdentity $
  (runReaderT $ view_ getterPointX) (makePoint (1,2))
-- >>


-- Lenses

type Lens_ s t a b =
  forall f. Functor f => (a -> f b) -> s -> f t

__1 :: Lens (a, t) (b, t) a b
__1 f (a1, a2) = (,a2) <$> f a1

__2 :: Lens (t, a) (t, b) a b
__2  f (a1, a2) = (a1,) <$> f a2

lens_ :: (s -> a) -> (s -> b -> t) -> Lens_ s t a b
lens_ f g = \h s ->
  let a = f s
      bt = g s
  in bt <$> (h a)

tuple1 = over_ __1 length ("You are sexy and you know it", 1)

tuple2 = set_ __2 7 (4, 1)

tuple3 = toListOf_ __1 (4, 1)

tuple4 = set_ __2 7 (4, 1)

pointLensX :: Lens_ Point Point Double Double
pointLensX = lens_ _positionX (\p x -> p { _positionX = x })

pointLensY :: Lens_ Point Point Double Double
pointLensY = lens_ _positionY (\p y -> p { _positionY = y })

segmentLensStart :: Lens_ Segment Segment Point Point
segmentLensStart = lens_ _segmentStart (\s p -> s { _segmentStart = p })

segmentLensEnd :: Lens_ Segment Segment Point Point
segmentLensEnd = lens_ _segmentEnd (\s p -> s { _segmentEnd = p })

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
