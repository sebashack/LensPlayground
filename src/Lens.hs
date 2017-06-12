{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}

module Lens where

import Segment

-- | Lens Type
type Lens s t a b =
  forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a


lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens f g h s =
  let a = f s
      bt = g s
  in bt <$> h a

_1 :: Lens (a, t) (b, t) a b
_1 f (a1, a2) = (,a2) <$> f a1

_2 :: Lens (t, a) (t, b) a b
_2  f (a1, a2) = (a1,) <$> f a2

pointLensX :: Lens Point Point Double Double
pointLensX = lens _positionX (\p x -> p { _positionX = x })

pointLensY :: Lens Point Point Double Double
pointLensY = lens _positionY (\p y -> p { _positionY = y })

segmentLensStart :: Lens Segment Segment Point Point
segmentLensStart = lens _segmentStart (\s p -> s { _segmentStart = p })

segmentLensEnd :: Lens Segment Segment Point Point
segmentLensEnd = lens _segmentEnd (\s p -> s { _segmentEnd = p })
