-- | An implementation of the state monad transformer.
-- Provides the ability to manipulate a piece of state.

module Monad.StateT 
  ( StateT, runState, evalState, execState, module Monad.Prelude
    -- * Examples
    -- $Examples
  ) where

import Monad.Prelude
import Control.Monad.Fix


-- | A computation that can manipulate a state of type /s/, 
-- may side-effect as described by /m/, 
-- and computes a value of type /a/, 
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

instance CollectorM m w => CollectorM (StateT s m) w where
  collect (S m)     = S (\s -> do ((a,s'),o) <- collect (m s)
                                  return ((a,o),s'))

instance Monad m => StateM (StateT s m) s where
  get               = S (\s -> return (s,s))
  set s             = S (\s1 -> return (s1,s))
  update f          = S (\s -> return (s,f s))

instance ExceptM m e => ExceptM (StateT s m) e where
  raise e           = lift (raise e)

instance HandlerM m e => HandlerM (StateT s m) e where
  handle (S m) h    = S (\s -> handle (m s) (runState s . h))

instance MonadPlus m => MonadPlus (StateT s m) where
  mzero             = lift mzero
  mplus (S m) (S n) = S (\s -> m s `mplus` n s) 

instance ContM m => ContM (StateT s m) where
  callcc m          = S (\s -> callcc $ \k -> 
                               runState s $ m $ \a -> lift (k (a,s)))
     

{- $Examples

Raising an exception undoes changes to the state.

> prop_StateT'HandlerM = test == Right (42,42)
>   where test  = runId $ runExcept $ runState 42
>               $ do set 17
>                    raise "Error"
>                  `handle_` get



Backtracking undoes changes to the state.

> prop_StateT'MonadPlus = test == Just (42,42)
>   where test = runId $ runSearchOne $ runState 42
>              $ do set 17
>                   mzero
>                 `mplus` get



Jumping to a continuation undoes changes to the state.

> prop_StateT'ContM = test == (42,42)
>   where test = runId $ runCont $ runState 42
>              $ do (stop,k) <- returnCC False
>                   if stop then get
>                           else do set 17
>                                   cJump True k

-}

