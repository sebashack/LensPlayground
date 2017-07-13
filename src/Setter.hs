{-# LANGUAGE RankNTypes  #-}

module Setter where

import Control.Monad.Identity
import Person

-- | Setter Type
type Setter s t a b = (a -> Identity b) -> s -> Identity t

-- | Utility Functions
over :: Setter s t a b -> (a -> b) -> s -> t
over setter g s = runIdentity $ setter g' s
  where
    g' = Identity . g

set :: Setter s t a b -> b -> s -> t
set setter v = over setter (const v)


-- | Some Setter Examples

-- | Traverse a Human affecting its super-powers:
--   (Maybe SuperPower -> Identity (Maybe (SuperPower, String))) -> Human -> Identity Human
--   When you affect a Human's super-power, you also affect its heroName
experimentWithHuman :: Setter Human Human (Maybe SuperPower) (Maybe (SuperPower, String))
experimentWithHuman g (Human name' age' gender' ethnicity' _ superPower') =
 let newPower = g superPower'
 in Human name' age' gender' ethnicity' <$> (fmap . fmap) snd newPower <*> (fmap . fmap) fst newPower


-- | Traverse a Person affecting its civil-status:
--   (CivilStatus -> Identity (CivilStatus, Maybe Human)) -> Person -> Identity Person
--   When you affect a person's civil-status you also affect their couple
updatePersonCivilStatus :: Setter Person Person CivilStatus (CivilStatus, Maybe Human)
updatePersonCivilStatus g (Person self' father' mother' children' civilStatus' _) =
  let newCivilStatus = g civilStatus'
  in Person self' father' mother' children' <$> fmap fst newCivilStatus <*> fmap snd newCivilStatus

-- | Traverse a Person affecting its children:
--   (Human -> Idetity Human) -> Person -> Identity Person
--   When you affect a person's civil-status you also affect their couple.
updatePersonChildren :: Setter Person Person [Human] [Human]
updatePersonChildren g person =
  updateChildren person  <$> g (children person)
  where
    updateChildren p newChildren = p { children = newChildren }

-- | Traverse a Person affecting their super-powers:
--   (Maybe SuperPower -> Identity (Maybe (SuperPower, String))) -> Person -> Identity Person
alterPersonSuperPowers :: Setter Person Person (Maybe SuperPower) (Maybe (SuperPower, String))
alterPersonSuperPowers g person =
  updateSelf person <$> experimentWithHuman g (self person)
  where
    updateSelf p newSelf = p { self = newSelf }

-- | Traverse a person affecting their children's super-powers
--   (Maybe SuperPower -> Identity (Maybe (SuperPower, String))) -> Person -> Identity Person
alterPersonChildrenSuperPowers :: Setter Person Person (Maybe SuperPower) (Maybe (SuperPower, String))
alterPersonChildrenSuperPowers g person =
  updateChildren  person <$> sequenceA (experimentWithHuman g <$> children person)
  where
    updateChildren p newChildren = p { children = newChildren }


-- | Value Examples

psychicOrElectro :: Maybe SuperPower -> Maybe (SuperPower, String)
psychicOrElectro Nothing = Just (Electromagnetism, "Electro Guy")
psychicOrElectro _ = Just (PsychicControl, "Psychic Guy")

speedcOrStrengh :: Maybe SuperPower -> Maybe (SuperPower, String)
speedcOrStrengh Nothing = Just (SuperSpeed, "Fast Guy")
speedcOrStrengh _ = Just (SuperStrengh, "Strong Guy")


person1WithPower :: Person
person1WithPower = over alterPersonSuperPowers psychicOrElectro person1

person2WithPower :: Person
person2WithPower = over alterPersonSuperPowers speedcOrStrengh person2

person1WithChildren :: Person
person1WithChildren =
  set updatePersonChildren [human6] person1WithPower

person2WithChildren :: Person
person2WithChildren =
  set updatePersonChildren [human7] person2WithPower

person1WithSuperChildren :: Person
person1WithSuperChildren =
  over alterPersonChildrenSuperPowers psychicOrElectro person1WithChildren

person2WithSuperChildren :: Person
person2WithSuperChildren =
  over alterPersonChildrenSuperPowers speedcOrStrengh person2WithChildren
