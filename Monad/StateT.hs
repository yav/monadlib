-- | An implementation of the state monad transformer.
-- Provides the ability to manipulate a piece of state.

module Monad.StateT 
  ( -- * Examples
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
evalState          :: Monad m => s -> StateT s m a -> m a
evalState s m       = fst # runState s m

-- | Execute a stateful computation just for the state.
execState          :: Monad m => s -> StateT s m a -> m s
execState s m       = snd # runState s m

                                        



instance Monad m => Functor (StateT s m) where
  fmap f m          = f # m

instance Monad m => Monad (StateT s m) where
  return a          = lift (return a)
  S m >>= k         = S (\s -> do (a,s1)  <- m s
                                  let S n  = k a
                                  n s1)
  S m >> S n        = S (\s -> do (_,s1) <- m s
                                  n s1)

                                   

instance Trans (StateT s) where
  lift m            = S (\s -> do x <- m
                                  return (x,s))

instance BaseM m b => BaseM (StateT s m) b where
  inBase m          = lift (inBase m)

instance MonadFix m => MonadFix (StateT s m) where
  mfix f            = S (\s -> mfix (\it -> let S m = f (fst it)
                                            in m s))

instance ReaderM m r => ReaderM (StateT s m) r where
  getR              = lift getR

instance ReadUpdM m r => ReadUpdM (StateT s m) r where
  setR x (S m)      = S (\s -> setR x (m s))
  updateR f (S m)   = S (\s -> updateR f (m s))

instance WriterM m w => WriterM (StateT s m) w where
  put o             = lift (put o)

-- Note: Currently the censor sees the changes of the state done by
-- the computation... Is that correct?
instance CollectorM m w => CollectorM (StateT s m) w where
  collect (S m)     = S (\s -> do ((a,s'),o) <- collect (m s)
                                  return ((a,o),s'))

instance Monad m => StateM (StateT s m) s where
  get               = S (\s -> return (s,s))
  set s             = S (\s1 -> return (s1,s))
  update f          = S (\s -> return (s,f s))


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
  handle (S m) h    = S (\s -> handle (m s) (runState s . h))

instance MonadPlus m => MonadPlus (StateT s m) where
  mzero             = lift mzero
  mplus (S m) (S n) = S (\s -> m s `mplus` n s) 

instance SearchM m => SearchM (StateT s m) where
  checkSearch (S m) = S (\s -> do x <- checkSearch (m s)
                                  case x of
                                    Nothing -> return (Nothing,s)
                                    Just ((a,s),xs) -> 
                                      return (Just (a,S (\_ -> xs)),s))

instance ContM m => ContM (StateT s m) where
  callcc m          = S (\s -> callcc $ \k -> 
                               runState s $ m $ \a -> lift (k (a,s)))
     

