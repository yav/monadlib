module Monad.Writer 
  ( Writer, runWriter, evalWriter, execWriter
  , module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix
import Data.Monoid


-- | A computation that computes a value of type /a/,
-- and can write values to a buffer of type /w/.
newtype Writer w a  = W { unW :: (a,w) }

-- | Execute computation, returning both its result and the collected output.
runWriter          :: Writer w a -> (a,w)
runWriter m         = unW m

-- | Execute a computation, ignoring its output.
evalWriter         :: Writer w a -> a
evalWriter m        = fst (runWriter m)

-- | Execute a computation just for the side effect.
execWriter         :: Writer w a -> w 
execWriter m        = snd (runWriter m)


instance Monoid w => Functor (Writer w) where
  fmap f m          = liftM f m

instance Monoid w => Monad (Writer w) where
  return a          = W (a,mempty)
  W (a,w1) >>= k    = W (let (b,w2) = unW (k a)
                             w      = w1 `mappend` w2
                         in seq w (b,w))

instance Monoid w => BaseM (Writer w) (Writer w) where inBase x = x

instance Monoid w => MonadFix (Writer w) where
  mfix f            = W (let r = unW (f (fst r)) in r)

instance Monoid w => WriterM (Writer w) w where
  put w             = seq w (W ((),w))

instance Monoid w => TakeWriterM (Writer w) w where
  takeFrom (W m)    = W (m,mempty)


