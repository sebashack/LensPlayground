{-# LANGUAGE RankNTypes  #-}

module Traversal where

import Segment
import Person
import Control.Monad (mapM)

-- | Traversal Type
type Traversal s t a b = forall f .
  Applicative f => (a -> f b) -> s -> f t


-- | Traversable and Traversal relationship:
-- traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' :: Traversable t => Traversal (t a) (t b) a b
traverse' = traverse


-- | Examples

-- | Traverse a Human affecting their super-powers:
--   Applicative f => (Maybe SuperPower -> f (Maybe (SuperPower, String))) -> Human -> f Human
--   When you affect a Human's super-power, you also affect its heroName
experimentWithHuman :: Traversal Human Human (Maybe SuperPower) (Maybe (SuperPower, String))
experimentWithHuman g (Human name' age' gender' ethnicity' _ superPower') =
 let newPower = g superPower'
 in Human name' age' gender' ethnicity' <$> (fmap . fmap) snd newPower <*> (fmap . fmap) fst newPower


-- | Traverse a Person affecting its civil-status:
--   Applicative f => (CivilStatus -> f (CivilStatus, Maybe Human)) -> Person -> f Person
--   When you affect a person's civil-status you also affect their couple.
updatePersonCivilStatus :: Traversal Person Person CivilStatus (CivilStatus, Maybe Human)
updatePersonCivilStatus g (Person self' father' mother' children' civilStatus' _) =
  let newCivilStatus = g civilStatus'
  in Person self' father' mother' children' <$> fmap fst newCivilStatus <*> fmap snd newCivilStatus


-- | Traverse a Person affecting their super-powers
--   Applicative f => (Maybe SuperPower -> f (Maybe (SuperPower, String))) -> Person -> f Person
alterPersonSuperPowers :: Traversal Person Person (Maybe SuperPower) (Maybe (SuperPower, String))
alterPersonSuperPowers g person =
  updateSelf person <$> experimentWithHuman g (self person)
  where
    updateSelf p newSelf = p { self = newSelf }

-- | Traverse a person affecting their children's super-powers
--   Applicative f => (Maybe SuperPower -> f (Maybe (SuperPower, String))) -> Person -> f Person
alterPersonChildrenSuperPowers :: Traversal Person Person (Maybe SuperPower) (Maybe (SuperPower, String))
alterPersonChildrenSuperPowers g person = updateChildren  person <$> sequenceA (experimentWithHuman g <$> children person)
  where
    updateChildren p newChildren = p { children = newChildren }






-- -- | Examples: Applicative f => (Double -> f Double) -> Point -> f Point
-- pointCoordinates :: Traversal Point Point Double Double
-- pointCoordinates g (Point x y) = Point <$> g x <*> g y

-- positionX :: Traversal Point Point Double Double
-- positionX g (Point x y) = Point <$> g x <*> pure y

-- positionY :: Traversal Point Point Double Double
-- positionY g (Point x y) = Point <$> pure x <*> g y

-- -- | Examples: Applicative f => (Double -> f Double) -> Segment -> f Segment
-- extremityCoordinates :: Traversal Segment Segment Double Double
-- extremityCoordinates g (Segment start end)
--   = Segment <$> pointCoordinates g start <*> pointCoordinates g end

-- segmentStart :: Traversal Segment Segment Double Double
-- segmentStart g (Segment start end) = Segment <$> pointCoordinates g start <*> pure end

-- segmentEnd :: Traversal Segment Segment Double Double
-- segmentEnd g (Segment start end) = Segment <$> pure start <*> pointCoordinates g end
