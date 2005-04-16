-- | Implements the writer (aka output) transformer.
-- Provides the ability to accumulate data in a buffer.
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
  , module Monad.Monoid
  ) where

import Monad.Prelude
import Control.Monad.Fix

import Monad.ReaderT
import Monad.Monoid

-- | A computation that computes a result of type /a/, may place items 
-- in a buffer of type /w/, and can also side-effect as described by /m/.
newtype WriterT w m a = W (ReaderT (MonoidOn w) m (a,w))

-- | Execute computation, getting its result and the collected output.
runWriter          :: Monad m => MonoidOn w -> WriterT w m a -> m (a,w)
runWriter d (W m)   = runReader d m

-- | Execute a computation, ignoring its output.
evalWriter         :: Monad m => MonoidOn w -> WriterT w m a -> m a
evalWriter d m     = fst # runWriter d m

-- | Execute a computation just for the side effect.
execWriter          :: Monad m => MonoidOn w -> WriterT w m a -> m w
execWriter d m       = snd # runWriter d m


instance Monad m => Functor (WriterT w m) where
  fmap f (W m)      = W (do ~(a,w) <- m
                            return (f a, w))

instance Monad m => Monad (WriterT w m) where
  return a          = W (do none <- mUnit # get
                            return (a,none))
  W m >>= k         = W (do ~(a,w1) <- m
                            let W m' = k a
                            ~(b,w2) <- m'
                            join <- mJoin # get
                            return (b,w1 `join` w2))
                        
instance Trans (WriterT w) where
  lift m            = W (do a <- lift m
                            none <- mUnit # get
                            return (a,none))

instance BaseM m b => BaseM (WriterT w m) b where
  inBase m          = lift (inBase m)

instance MonadFix m => MonadFix (WriterT w m) where
  mfix f            = W (mdo let W m = f (fst r) 
                             r <- m
                             return r)
                        
instance ReaderM m r => ReaderM (WriterT w m) r where
  get               = lift get
  local f (W m)     = W (do mon <- get
                            lift (local f (runReader mon m)))


instance Monad m => WriterM (WriterT w m) w where
  put w             = W (return ((), w))

instance Monad m => TakeWriterM (WriterT w m) w where
  takeFrom (W m)    = W (do r <- m
                            none <- mUnit # get
                            return (r,none))

instance StateM m s => StateM (WriterT w m) s where
  peek              = lift peek
  poke s            = lift (poke s)


-- $ExceptM
-- Exceptions undo the output.
--
-- see: runWriterOut in <Examples/Except.hs>
instance ExceptM m e => ExceptM (WriterT w m) e where
  raise e           = lift (raise e)
  handle (W m) h    = W (m `handle` (\e -> let W m' = h e in m'))

-- $BackM
-- Backtracking undoes the output.  
-- Another way to put this is that every alternative has its own output buffer.
--
-- see: runWriterOut in <Examples/Back.hs>
instance MonadPlus m => MonadPlus (WriterT w m) where
  mzero               = lift mzero
  mplus (W m1) (W m2) = W (m1 `mplus` m2)


-- $ContM
-- Jumping undoes changes to the output.
--
-- see: runWriterOut in <Examples/Cont.hs>
instance ContM m => ContM (WriterT w m) where
  callcc m          = W (callcc (\k -> 
                           let W m' = m (\a -> W (do none <- mUnit # get
                                                     k (a,none)))
                           in m'))




