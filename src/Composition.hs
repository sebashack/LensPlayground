module Composition where

import Traversal (experimentWithHuman)
import Person
import Iso (makeHumanIso, humanTuple, HTuple)
import Setter (set)

-- | Traversal Type: Traversal Human Human (Maybe SuperPower) (Maybe (SuperPower, String))
--   Applicative f => (Maybe SuperPower -> f (Maybe (SuperPower, String))) -> Human -> f Human

-- | Iso Type: Iso HTuple HTuple Human Human
--   forall p f . (Profunctor p, Functor f) => p Human (f Human) -> p HTuple (f HTuple)
--   specialized version:  Human -> (f Human) -> HTuple -> (f HTuple)


setPowerTup :: Maybe (SuperPower, String) -> HTuple -> HTuple
setPowerTup = set (makeHumanIso . experimentWithHuman)

powerTup :: IO HTuple
powerTup = setPowerTup (Just (CyborgBody, "Mr. Android")) <$> humanTuple
