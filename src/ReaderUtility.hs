module ReaderUtility where

import Control.Monad.Reader


ask' :: Monad m => ReaderT a m a
ask' = ReaderT return

asks' :: Monad m => (r -> a) -> ReaderT r m a
asks' f = ReaderT $ return . f
