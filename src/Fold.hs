{-# LANGUAGE RankNTypes  #-}

module Fold where

import Getting
import Segment
import Control.Applicative
import Data.Monoid
import Control.Monad.Reader
import  ReaderUtility (asks')

-- | Fold type
type Fold s a = forall m .
  Monoid m => Getting m s a

-- | foldMapOf :: Monoid r => Fold s a -> (a -> r) -> s -> r
-- | foldMapOf :: Monoid r => Fold' s a -> (a -> r) -> s -> r
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f s = getConst (g s)
  where
    g = l (Const . f)

toListOf :: Getting [a] s a -> s -> [a]
toListOf l = foldMapOf l (:[])

preview :: Monad m => Getting (First a) s a -> ReaderT s m (Maybe a)
preview l = asks' $ getFirst . foldMapOf l (First . Just)

-- | pointGetting :: Getting (Sum Double) Point Double
pointFold :: Fold Point Double
pointFold g (Point x y) = Const (getConst $ g (x + y))

-- | segmentGetting :: Monoid m => Getting m Segment Double
segmentFold :: Fold Segment Double
segmentFold g (Segment start end) =
  Const $ getConst $ g $ getSum $ foldPoint start <> foldPoint end
  where
    foldPoint = foldMapOf pointFold Sum
