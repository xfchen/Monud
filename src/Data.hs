module Data where

newtype UpdateT m a = UpdateT {runUpdateT :: m (Update a)}

instance Monad m => Monad (UpdateT m) where
          return :: (Monad m) => a -> UpdateT (m a)
          return a = UpdateT $ return (return a)
          >>= :: (Monad m) => UpdateT m a -> (a -> UpdateT m b) -> UpdateT m b 
          x >>= f = UpdateT do
                      unrappad <- runUpdateT $ f x
                      runUpdateT runUpdateT (f y) unwrapped
                      
