{-# LANGUAGE RankNTypes #-}

module Iso where

import Segment
import Data.Functor.Contravariant (Contravariant, (>$<), contramap)
import Data.Profunctor
import Data.Functor.Identity

-- | Exchange an ContraExchange types which are useful to define Isos
data Exchange a b s t = Exchange (s -> a) (b -> t)

data ContraExchange a b t s = ContraExchange (s -> a) (b -> t)

instance Functor (Exchange a b s) where
  fmap f (Exchange sa bt) = Exchange sa (f . bt)

instance Contravariant (ContraExchange a b t) where
  contramap f (ContraExchange sa bt) = ContraExchange (sa . f) bt

fromContraEx :: ContraExchange a b t s -> Exchange a b s t
fromContraEx (ContraExchange sa bt) = Exchange sa bt

toContraEx :: Exchange a b s t -> ContraExchange a b t s
toContraEx (Exchange sa bt) = ContraExchange sa bt

-- | Profuctor (Exchange a b)
instance Profunctor (Exchange a b) where
  lmap f ex = fromContraEx $ f >$< toContraEx ex
  rmap f ex = f <$> ex
  dimap f g ex =  fromContraEx  $ f >$< toContraEx (g <$> ex)

-- | Iso Type
type Iso s t a b =
  forall p f . (Profunctor p, Functor f) => p a (f b) -> p s (f t)

type AnIso s t a b =
  (Exchange a b) a (Identity b) -> (Exchange a b) s (Identity t)

iso :: (s -> a) -> (b -> t) -> Iso s t a b
iso sa bt = dimap sa (fmap bt)

withIso :: AnIso s t a b -> ((s -> a) -> (b -> t) -> r) -> r
withIso ai k =
  let Exchange sa bt = ai $ Exchange id Identity
  in k sa (runIdentity . bt)

from :: AnIso s t a b -> Iso b a t s
from ai = withIso ai $ \sa bt -> iso bt sa

unmakePointIso :: Iso Point Point (Double, Double) (Double, Double)
unmakePointIso = iso unmakePoint makePoint

makePointIso :: Iso (Double, Double) (Double, Double) Point Point
makePointIso = from unmakePointIso
