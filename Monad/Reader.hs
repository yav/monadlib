module Monad.Reader 
  ( Reader
  , run
  , get, local, localSet
  ) where

import Control.Monad.Fix

newtype Reader r a  = R (r -> a)

instance Functor (Reader r) where
  fmap f (R m)      = R (\r -> f (m r))

instance Monad (Reader r) where
  return x          = R (\_ -> x)
  R m >>= k         = R (\r -> let R n = k (m r)
                               in n r)

instance MonadFix (Reader r) where
  mfix f            = R (\r -> let R n = f a
                                   a   = n r
                               in a)
                           
run                :: r -> Reader r a -> a
run r (R m)         = m r 

get                :: Reader r r
get                 = R (\r -> r)

local              :: (r -> s) -> Reader s a -> Reader r a  
local f (R m)       = R (\r -> m (f r))

localSet           :: r -> Reader r a -> Reader s a
localSet r (R m)    = R (\_ -> m r)


