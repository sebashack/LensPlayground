{-# LANGUAGE RankNTypes #-}

module Prism where

import Data.Profunctor hiding (Choice)
import Setter (over)
import Person

class Profunctor p => Choice p where
  left :: p a b -> p (Either a c) (Either b c)
  right :: p a b -> p (Either c a) (Either c b)

type Prism s t a b = forall p f .
  (Choice p, Applicative f) => p a (f b) -> p s (f t)

-- | If we have a succesful target (b -> t) returns a signal of success.
--   (s -> Either t a) is a function which access the whole with possibility of error.
--   The signal error is of type t.
prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
prism bt sEa = dimap sEa (r bt) . right
  where
    r :: Applicative f => (b -> t) -> Either t (f b) -> f t
    r f = either pure (fmap f)

_Right :: Prism (Either c a) (Either c b) a b
_Right = prism Right choiceFunc
  where
    choiceFunc :: Either a c -> Either (Either a b) c
    choiceFunc = either (Left . Left) Right

_Just :: Prism (Maybe a) (Maybe b) a b
_Just = prism Just choiceFunc
  where
    choiceFunc :: Maybe b -> Either (Maybe a) b
    choiceFunc = maybe (Left Nothing) Right


-- | Value examples
instance Choice (->) where
  left f (Left a) = Left $ f a
  left _ (Right c) = Right c

  right f (Right a) = Right $ f a
  right _ (Left c) = Left c


-- | Prisms are Lenses, and Traversals
electroCouple :: Maybe Human
electroCouple = over _Just (\h -> h { superPower = Just Electromagnetism}) (couple person1)

electroNothing :: Maybe Human
electroNothing = over _Just (\h -> h { superPower = Just Electromagnetism}) (couple person3)
