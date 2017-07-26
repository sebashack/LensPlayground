{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TupleSections   #-}

module Lens where

import Person
import Control.Monad.Reader
import Setter (set)
import Getter (view)

-- | Lens Type
type Lens s t a b =
  forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

-- Get a Lens from a getter and a setter function
lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens f g h s =
  let a = f s
      bt = g s
  in bt <$> h a


-- | Value examples
personCouple :: Lens Person Person (Maybe Human) (Maybe Human)
personCouple = lens couple marryOrDivorce
  where
    marryOrDivorce :: Person -> Maybe Human -> Person
    marryOrDivorce person Nothing = person { couple = Nothing, civilStatus = Divorced }
    marryOrDivorce person human = person { couple = human, civilStatus = Married }

marriagedPerson :: Person
marriagedPerson = set personCouple (Just human6) person3


-- A Lens is a Getter
aPersonCouple :: IO (Maybe Human)
aPersonCouple = (runReaderT $ view personCouple) marriagedPerson
