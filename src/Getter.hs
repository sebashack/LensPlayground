{-# LANGUAGE RankNTypes  #-}

module Getter where

import Getting
import Person
import Setter
import Data.Monoid
import Control.Monad.Reader
import ReaderUtility (asks')
import Control.Applicative

-- | Getting Type
type Getter s a = forall r . Getting r s a


to :: (s -> a) -> Getter s a
to f g s = Const . getConst $ g (f s)

view :: Monad m => Getting a s a -> ReaderT s m a
view l = asks' $ getConst . l Const


-- | Value examples
getHeroNames :: Human -> String
getHeroNames (Human name' _ _ _ Nothing _) = name'
getHeroNames (Human name' _ _ _ (Just heroName') _) = name' <> ", alias: " <> heroName'

heroNamesGetter :: Getter Person String
heroNamesGetter = to $ getHeroNames . self

aHeroNames :: IO String
aHeroNames = (runReaderT $ view heroNamesGetter) person1WithPower
