-- | The identity monad.
module Monad.Id (Id, runId, module Monad.Prelude) where

import Monad.Prelude
import Control.Monad.Fix

-- | A computation that will produce a value of type /a/.
newtype Id a        = Id a

-- | Run a computation.
runId              :: Id a -> a
runId (Id a)        = a

instance Functor Id where
  fmap f (Id a)     = Id (f a)

instance Monad Id where
  return x          = Id x
  Id x >>= k        = k x

instance BaseM Id Id where inBase x = inBase x

instance MonadFix Id where
  mfix f            = let r@(Id x) = f x
                      in r




