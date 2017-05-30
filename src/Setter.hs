{-# LANGUAGE RankNTypes  #-}

module Setter where

import Control.Monad.Identity


-- | Setter Type
type Setter s t a b = (a -> Identity b) -> s -> Identity t

-- | Utility Functions
over :: Setter s t a b -> (a -> b) -> s -> t
over setter g s = runIdentity $ setter g' s
  where
    g' = Identity . g

set :: Setter s t a b -> b -> s -> t
set setter v = over setter (const v)
