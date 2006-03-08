-- | An implementation of the state monad transformer.
-- Provides the ability to manipulate a piece of state.
--
-- 
-- * Commutes with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT"
-- 
-- * Does not commute with: "Monad.ExceptT"
module Monad.StateT 
  ( -- * Instance notes

    -- ** instance 'HandlerM'
    -- $HandlerM

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

-- | Execute a stateful computation ignoring the final state.
evalState          :: Functor m => s -> StateT s m a -> m a
evalState s m       = fmap fst (runState s m)

-- | Execute a stateful computation just for the state.
execState          :: Functor m => s -> StateT s m a -> m s
execState s m       = fmap snd (runState s m)

                                        



instance Functor m => Functor (StateT s m) where
  fmap f (S m)      = S (\s -> fmap (\ (a,s1) -> (f a, s1)) (m s))

instance Monad m => Monad (StateT s m) where
  return a          = S (\s -> return (a,s))
  S m1 >>= k        = S (\s -> do (a,s1)  <- m1 s
                                  let S m2  = k a
                                  m2 s1)

instance Trans (StateT s) where
  lift m            = S (\s -> do x <- m
                                  return (x,s))

instance BaseM m b => BaseM (StateT s m) b where
  inBase m          = lift (inBase m)

instance MonadFix m => MonadFix (StateT s m) where
  mfix f            = S (\s -> mfix (\it -> let S m = f (fst it)
                                            in m s))

instance MonadPlus m => MonadPlus (StateT s m) where
  mzero             = lift mzero
  mplus (S f) (S g) = S (\s -> mplus (f s) (g s))


instance ReaderM m r => ReaderM (StateT s m) r where
  getR              = lift getR

instance ReadUpdM m r => ReadUpdM (StateT s m) r where
  updateR f m       = do s <- get
                         let m' = runState s m
                         (a,s) <- lift (updateR f m')
                         set s
                         return a

instance WriterM m w => WriterM (StateT s m) w where
  put o             = lift (put o)

instance CollectorM m w => CollectorM (StateT s m) w where
  collect m         = do s <- get
                         let m' = runState s m
                         ((a,s'),o) <- lift (collect m')
                         set s'
                         return (a,o)

instance Monad m => StateM (StateT s m) s where
  get               = S (\s -> return (s,s))
  set s             = S (\s1 -> return (s1,s))


instance ExceptM m e => ExceptM (StateT s m) e where
  raise e           = lift (raise e)

-- $HandlerM
-- Raising an exception undoes changes to the state.  For example:
--
-- > test = runId $ runExcept $ runState 42 
-- >      $ withHandler (\_ -> get) 
-- >      $ do set 17
-- >           raise "Error"
-- 
-- produces @Right (42,42)@

instance HandlerM m e => HandlerM (StateT s m) e where
  handle m h        = do s <- get
                         let m'   = runState s m
                             h' e = runState s (h e)
                         (a,s) <- lift (handle m' h')
                         set s
                         return a


