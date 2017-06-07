{-# LANGUAGE RankNTypes  #-}

module Getter where

import Getting
import Segment
import Control.Monad.Reader
import ReaderUtility (asks')
import Control.Applicative

-- | Getting Type
type Getter s a = forall r . Getting r s a


to :: (s -> a) -> Getter s a
to f g s = Const . getConst $ g (f s)

view :: Monad m => Getting a s a -> ReaderT s m a
view l = asks' $ getConst . l Const

getterPointX :: Getting Double Point Double
getterPointX = to _positionX

getterPointY :: Getting Double Point Double
getterPointY = to _positionY
