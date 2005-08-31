-- | Implements the writer (aka output) transformer.
-- Provides the ability to accumulate data in a buffer.
-- This implementation is strict in the buffer.
--
-- * Commutes with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT"
-- 
-- * Does not commute with: "Monad.ExceptT", "Monad.BackT", "Monad.ContT"
module Monad.WriterT 
  ( -- * Instance notes
  
    -- ** instance ExceptM
    -- $ExceptM

    -- ** instance BackM 
    -- $BackM

    -- ** instance ContM
    -- $ContM
 
  WriterT, runWriter, evalWriter, execWriter, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix
import Data.Monoid

-- | A computation that computes a result of type /a/, may place items 
-- in a buffer of type /w/, and can also side-effect as described by /m/.
newtype WriterT w m a = W { unW :: m (a,w) }

-- | Execute a computation, returns both the result and the collected output.
runWriter          :: WriterT w m a -> m (a,w)
runWriter (W m)     = m

-- | Execute a computation, ignoring the output.
evalWriter         :: Monad m => WriterT w m a -> m a
evalWriter m        = fst # runWriter m

-- | Execute a computation just for the side effect.
execWriter          :: Monad m => WriterT w m a -> m w
execWriter m        = snd # runWriter m


instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap f m          = liftM f m

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return a          = lift (return a)
  W m >>= k         = W (do (a,w1) <- m
                            (b,w2) <- unW (k a)
                            let w = w1 `mappend` w2
                            seq w (return (b,w)))
                            -- return (b,w))
                        
instance Monoid w => Trans (WriterT w) where
  lift m            = W (do a <- m
                            return (a,mempty))

instance (Monoid w, BaseM m b) => BaseM (WriterT w m) b where
  inBase m          = lift (inBase m)

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
  mfix f            = W (mfix (\r -> unW (f (fst r))))
                        
instance (Monoid w, ReaderM m r) => ReaderM (WriterT w m) r where
  get               = lift get
  local f (W m)     = W (local f m)

instance (Monoid w, Monad m) => WriterM (WriterT w m) w where
  put w             = W (seq w (return ((), w)))
  -- put w             = W (return ((), w))

instance (Monoid w, Monad m) => TakeWriterM (WriterT w m) w where
  takeFrom (W m)    = W (do r <- m
                            return (r,mempty))

instance (Monoid w, StateM m s) => StateM (WriterT w m) s where
  peek              = lift peek
  poke s            = lift (poke s)


-- $ExceptM
-- Exceptions undo the output.
--
-- see: runWriterOut in <Examples/Except.hs>
instance (Monoid w, ExceptM m e) => ExceptM (WriterT w m) e where
  raise e           = lift (raise e)
  handle (W m) h    = W (m `handle` (\e -> unW (h e)))

-- $BackM
-- Backtracking undoes the output.  
-- Another way to put this is that every alternative has its own output buffer.
--
-- see: runWriterOut in <Examples/Back.hs>
instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero               = lift mzero
  mplus (W m1) (W m2) = W (m1 `mplus` m2)


-- $ContM
-- Jumping undoes changes to the output.
--
-- see: runWriterOut in <Examples/Cont.hs>
instance (Monoid w, ContM m) => ContM (WriterT w m) where
  callcc m          = W (callcc (\k -> unW (m (\a -> W (k (a,mempty))))))



