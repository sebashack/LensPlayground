{-# LANGUAGE RankNTypes  #-}

module Getting where

import Control.Applicative

-- newtype Const a b = Const { getConst :: a }

-- | Getting Type
type Getting r s a = (a -> Const r a) -> s -> Const r s
