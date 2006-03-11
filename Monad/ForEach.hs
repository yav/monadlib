module Monad.ForEach where

import Control.Monad

-- | Apply a monadic function to each element in a container.
-- In theory speak, this is a class that identifies functors which
-- distribute over all monads.
class Functor f => ForEach f where
  forEach            :: Monad m => f a -> (a -> m b) -> m (f b)

  -- | Apply a function to each element, ignoring the results.
  forEach_           :: Monad m => f a -> (a -> m b) -> m ()
  forEach_ xs f       = forEach xs f >> return ()

instance ForEach [] where
  forEach xs f        = mapM f xs

  forEach_ [] _       = return ()
  forEach_ (x:xs) f   = f x >> forEach_ xs f

instance ForEach Maybe where
  forEach Nothing _   = return Nothing
  forEach (Just x) f  = Just `liftM` f x







                          
                     

