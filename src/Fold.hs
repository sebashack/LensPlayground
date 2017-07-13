{-# LANGUAGE RankNTypes  #-}

module Fold where

import Getting
import Setter (person1WithSuperChildren, person2WithSuperChildren)
import Control.Applicative
import Data.Monoid
import Data.List (unwords)
import Data.Maybe (maybe, fromMaybe)
import Control.Monad.Reader
import ReaderUtility (asks')
import Person


-- | Fold type
type Fold s a = forall r .
  Monoid r => Getting r s a

-- | foldMapOf :: Monoid r => Fold s a -> (a -> r) -> s -> r
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf l f s = getConst (g s)
  where
    g = l (Const . f)

toListOf :: Getting [a] s a -> s -> [a]
toListOf l = foldMapOf l (:[])

preview :: Monad m => Getting (First a) s a -> ReaderT s m (Maybe a)
preview l = asks' $ getFirst . foldMapOf l (First . Just)


-- | Fold Examples

-- | Collect the names of all family members of a superhero
--   familyNames :: Fold Person Human
familyNames :: Getting String Person Human
familyNames g (Person _ father' mother' children' _ couple') =
  let fa = "Father: " <> getConst (g father') <> ", "
      mo = "Mother: "  <> getConst (g mother') <> ", "
      co = maybe "" (("Couple: " <>) . getConst) (g <$> couple') <> ", "
      cs = "Children: " <> unwords (fmap (getConst . g) children')
  in Const (fa <> mo <> co <> cs)

familyMembers :: Getting [Human] Person Human
familyMembers g (Person _ father' mother' children' _ couple') =
  let fa = getConst (g father')
      mo = getConst (g mother')
      co = fromMaybe [] (getConst . g <$> couple')
      cs = concat $ (getConst . g) <$> children'
  in Const (fa <> mo <> co <> cs)

previewHero :: Getting (First Human) Person Human
previewHero g (Person self' father' mother' children' _ couple') =
  let sf = getConst (g self')
      fa = getConst (g father')
      mo = getConst (g mother')
      co = fromMaybe (First Nothing) (getConst . g <$> couple')
      cs = mconcat $ (getConst . g) <$> children'
  in Const (sf <> fa <> mo <> co <> cs)


-- | Value examples
person1FamMembers :: String
person1FamMembers = foldMapOf familyNames name person1WithSuperChildren

person2FamMembers :: String
person2FamMembers = foldMapOf familyNames name person2WithSuperChildren

person1FamMembers' :: [Human]
person1FamMembers' = toListOf familyMembers person1WithSuperChildren

person2FamMembers' :: [Human]
person2FamMembers' = toListOf familyMembers person2WithSuperChildren

previewPerson1 :: IO (Maybe Human)
previewPerson1 = (runReaderT $ preview previewHero) person1WithSuperChildren

previewPerson2 :: IO (Maybe Human)
previewPerson2 = (runReaderT $ preview previewHero) person2WithSuperChildren
