-- | An implementation of the state monad transformer.
-- Provides the ability to manipulate a piece of state.
--
-- * Commutes with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT"
-- 
-- * Does not commute with: "Monad.ExceptT", "Monad.BackT", "Monad.ContT"
module Monad.StateT 
  ( -- * Instance notes

    -- ** instance ExceptM
    -- $ExceptM

    -- ** instance BackM
    -- $BackM

    -- ** instance ContM
    -- $ContM

  StateT, runState, evalState, execState, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix


-- | A computation that computes a value of type /a/, 
-- can manipulate a state of type /s/, and can also side-effect
-- as described by /m/.
newtype StateT s m a  = S (s -> m (a,s))

-- | Execute a stateful computation, as a result we get
-- the result of the computation, and the final state.
runState           :: s -> StateT s m a -> m (a,s)
runState s (S m)    = m s

-- | Execute a stateful computation, ignoring the final state.
evalState          :: Functor m => s -> StateT s m a -> m a
evalState s m       = fmap fst (runState s m)

-- | Execute a stateful computation, just for the side effect.
execState          :: Functor m => s -> StateT s m a -> m s
execState s m       = fmap snd (runState s m)

                                        



instance Functor m => Functor (StateT s m) where
  fmap f (S m)      = S (\s -> fmap (\ ~(a,s1) -> (f a, s1)) (m s))

instance Monad m => Monad (StateT s m) where
  return a          = S (\s -> return (a,s))
  S m1 >>= k        = S (\s -> do ~(a,s1)  <- m1 s
                                  let S m2  = k a
                                  m2 s1)

instance Trans (StateT s) where
  lift m            = S (\s -> do x <- m
                                  return (x,s))

instance MonadFix m => MonadFix (StateT s m) where
  mfix f            = S (\s -> mfix (\it -> let S m = f (fst it)
                                            in m s))


instance ReaderM m r => ReaderM (StateT s m) r where
  get               = lift get
  local f m         = do s <- peek
                         let m' = runState s m
                         ~(a,s) <- lift (local f m')
                         poke s
                         return a

instance WriterM m w => WriterM (StateT s m) w where
  put o             = lift (put o)

instance TakeWriterM m w => TakeWriterM (StateT s m) w where
  takeFrom m        = do s <- peek
                         let m' = runState s m
                         ~(~(a,s'),o) <- lift (takeFrom m')
                         poke s'
                         return (a,o)

instance Monad m => StateM (StateT s m) s where
  peek              = S (\s -> return (s,s))
  poke s            = S (\s1 -> return (s1,s))


-- $ExceptM
-- Raising an exception undoes changes to the state.
--
-- see: runStateOut in <Examples/Except.hs>
instance ExceptM m e => ExceptM (StateT s m) e where
  raise e           = lift (raise e)
  handle m h        = do s <- peek
                         let m'   = runState s m
                             h' e = runState s (h e)
                         ~(a,s) <- lift (handle m' h')
                         poke s
                         return a


-- $BackM
-- Backtracking undoes changes to the state.
-- Another way to put this is that every alternative has its own heap.
--
-- see: runStateOut in <Examples/Back.hs>
instance MonadPlus m => MonadPlus (StateT s m) where
  mzero             = lift mzero
  mplus m1 m2       = do s <- peek
                         let m1' = runState s m1
                             m2' = runState s m2
                         ~(a,s') <- lift (mplus m1' m2')
                         poke s'
                         return a


-- $ContM
-- Jumping undoes changes to the state.
--
-- see: runStateOut in <Examples/Cont.hs>
instance ContM m => ContM (StateT s m) where
  callcc m          = S (\s -> callcc (\k -> let S m' = m (\a -> lift (k (a,s)))
                                             in m' s))





