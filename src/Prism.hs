{-# LANGUAGE RankNTypes #-}

module Prism where

import Data.Profunctor hiding (Choice)

class Profunctor p => Choice p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)

type Prism s t a b = forall p f .
  (Choice p, Applicative f) => p a (f b) -> p s (f t)

prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sEa = dimap sEa (r bt) . right
  where
    r :: Applicative f => (b -> t) -> Either t (f b) -> f t
    r f = either pure (fmap f)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right (either (Left . Left) Right)

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just (maybe (Left Nothing) Right)
