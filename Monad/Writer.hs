-- | An implementation of the writer (aka output) monad.
module Monad.Writer (Writer, runWriter, evalWriter, execWriter, module Monad.Prelude) where

import Monad.Prelude
import Control.Monad.Fix


-- | A computation that computes a value of type /a/,
-- and can write /w/ values to a buffer.
newtype Writer w a  = W (a,[w] -> [w])

-- | Execute computation, getting its result and the collected output.
runWriter          :: Writer w a -> (a,[w])
runWriter (W ~(a,w))= (a,w [])

-- | Execute a computation, ignoring its output.
evalWriter         :: Writer w a -> a
evalWriter m        = fst (runWriter m)

-- | Execute a computation just for the side effect.
execWriter         :: Writer w a -> [w]
execWriter m        = snd (runWriter m)


instance Functor (Writer w) where
  fmap f (W ~(a,w)) = W (f a,w)

instance Monad (Writer w) where
  return a          = W (a,id)
  W ~(a,w1) >>= k   = W (let W (b,w2) = k a in (b, w1 . w2))

instance BaseM (Writer w) (Writer w) where inBase x = x

instance MonadFix (Writer w) where
  mfix f            = W (let W r@(a,w) = f a in r)

instance WriterM (Writer w) w where
  put w               = W ((),(w:))

instance TakeWriterM (Writer w) w where
  takeFrom (W ~(a,w)) = W ((a,w []), id)

