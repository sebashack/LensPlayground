{-# LANGUAGE RankNTypes  #-}

module Getting where

import Control.Applicative


-- | Getting Type
type Getting r s a = (a -> Const r a) -> s -> Const r s
