module Monad.Except
  ( Except (..)
  , raise, handle
  ) where

import Control.Monad.Fix

data Except e a     = Ok a | Fail e

instance Functor (Except e) where
  fmap f (Ok a)     = Ok (f a)
  fmap f (Fail e)   = Fail e

instance Monad (Except e) where
  return a          = Ok a
  Ok a >>= k        = k a
  Fail e >>= _      = Fail e

instance MonadFix (Except e) where
  mfix f            = let it@(Ok a) = f a
                      in it

-- | A computation that raises an exception.
raise              :: e -> Except e a
raise e             = Fail e

-- | Handle an exception if one occurs, or do nothing.
handle             :: Except e a -> (e -> Except e a) -> Except e a
handle it@(Ok a) _  = it
handle (Fail e) h   = h e






