
module Monad.Writer (Writer, runWriter, evalWriter, execWriter, 
      module Monad.Prelude) where

import Monad.Prelude
import Control.Monad.Fix

import Monad.Reader
import Monad.Monoid



-- | A computation that computes a value of type /a/,
-- and can write values to a buffer of type /w/.
newtype Writer w a  = W (Reader (MonoidOn w) (a,w))

-- | Execute computation, getting its result and the collected output.
runWriter          :: MonoidOn w -> Writer w a -> (a,w)
runWriter d (W m)   = runReader d m

-- | Execute a computation, ignoring its output.
evalWriter         :: MonoidOn w -> Writer w a -> a
evalWriter d m      = fst (runWriter d m)

-- | Execute a computation just for the side effect.
execWriter         :: MonoidOn w -> Writer w a -> w 
execWriter d m      = snd (runWriter d m)


instance Functor (Writer w) where
  fmap f (W m)      = W (do ~(a,w) <- m
                            return (f a,w)) 

instance Monad (Writer w) where
  return a          = W (do none <- mUnit # get
                            return (a,none))
  W m1 >>= k        = W (do ~(a,w1) <- m1
                            let W m2 = k a
                            ~(b,w2) <- m2
                            join    <- mJoin # get
                            return (b, join w1 w2))

instance BaseM (Writer w) (Writer w) where inBase x = x

instance MonadFix (Writer w) where
  mfix f            = W (mdo let W m = f (fst r)
                             r <- m
                             return r)

instance WriterM (Writer w) w where
  put w             = W (return ((),w))

instance TakeWriterM (Writer w) w where
  takeFrom (W m)    = W (do x <- m
                            none <- mUnit # get
                            return (x,none))


