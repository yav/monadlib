module Monad.Id 
  ( Id
  , run
  ) where

import Control.Monad.Fix

newtype Id a        = Id a

instance Functor Id where
  fmap f (Id a)     = Id (f a)

instance Monad Id where
  return x          = Id x
  Id x >>= k        = k x

instance MonadFix Id where
  mfix f            = let r@(Id x) = f x
                      in r

run                :: Id a -> a
run (Id a)          = a



