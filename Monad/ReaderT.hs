module Monad.ReaderT 
  ( ReaderT 
  , run
  , get, local, localSet
  ) where

import Control.Monad.Fix

newtype ReaderT r m a = R (r -> m a) 

instance Functor m => Functor (ReaderT r m) where
  fmap f (R m)      = R (\r -> fmap f (m r))

instance Monad m => Monad (ReaderT r m) where
  return a          = R (\r -> return a)
  R m1 >>= k        = R (\r -> do a <- m1 r
                                  let R m2 = k a
                                  m2 r)

instance MonadFix m => MonadFix (ReaderT r m) where
  mfix f            = R (\r -> mfix (\a -> let R m = f a 
                                           in m r))

run                :: r -> ReaderT r m a -> m a
run r (R m)         = m r

get                :: Monad m => ReaderT r m r
get                 = R (\r -> return r)

local              :: (r -> s) -> ReaderT s m a -> ReaderT r m a
local f (R m)       = R (\r -> m (f r))

localSet           :: r -> ReaderT r m a -> ReaderT s m a
localSet r (R m)    = R (\_ -> m r)

