{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

module Iso where

import Data.Functor.Contravariant (Contravariant, (>$<), contramap)
import Person
import Control.Monad.Reader
import Data.Profunctor
import Data.Functor.Identity
import Getter (view)

-- | Exchange an ContraExchange types which are useful to define Isos
--   It like the container for two isomorphic functions
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


-- | Value examples
type HTuple = ( String
              , Int
              , Gender
              , Ethnicity
              , Maybe String
              , Maybe SuperPower)

makeHuman :: HTuple -> Human
makeHuman (name', age', gender', eth', heroNm', supPower') =
  Human name' age' gender' eth' heroNm' supPower'

unmakeHuman :: Human -> HTuple
unmakeHuman Human{..} = (name, age, gender, ethnicity, heroName, superPower)

unmakeHumanIso :: Iso Human Human HTuple HTuple
unmakeHumanIso = iso unmakeHuman makeHuman

makeHumanIso :: Iso HTuple HTuple Human Human
makeHumanIso = from unmakeHumanIso

-- An Iso is a Lens and thus a Getter
humanTuple :: IO HTuple
humanTuple = (runReaderT $ view unmakeHumanIso) human2

aHuman :: IO Human
aHuman = humanTuple >>= runReaderT (view makeHumanIso)
