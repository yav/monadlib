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
evalState          :: Functor m => s -> StateT s m a -> m a
evalState s m       = fmap fst (runState s m)

-- | Execute a stateful computation just for the state.
execState          :: Functor m => s -> StateT s m a -> m s
execState s m       = fmap snd (runState s m)

                                        



instance Monad m => Functor (StateT s m) where
  fmap f m          = do x <- m
                         return (f x)

instance Monad m => Monad (StateT s m) where
  return a          = lift (return a)
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

instance ReaderM m r => ReaderM (StateT s m) r where
  getR              = lift getR

instance ReadUpdM m r => ReadUpdM (StateT s m) r where
  updateR f m       = do s <- get
                         let m' = runState s m
                         lift' (updateR f m')

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
                         lift' $ handle (runState s m)
                               $ \e -> runState s (h e)

instance MonadPlus m => MonadPlus (StateT s m) where
  mzero             = lift mzero
  mplus m n         = do s <- get
                         (a,s) <- lift (runState s m `mplus` runState s n)
                         set s
                         return a


instance SearchM m => SearchM (StateT s m) where
  checkSearch m     = do s <- get 
                         x <- lift $ checkSearch $ runState s m
                         case x of
                           Nothing -> return Nothing
                           Just ((a,s),xs) -> do set s
                                                 return (Just (a,lift' xs))

instance ContM m => ContM (StateT s m) where
  callcc m          = do s <- get
                         lift' $ callcc $ \k -> 
                                 runState s $ m $ \a -> lift (k (a,s))
     
lift' m = do (a,s) <- lift m
             set s
             return a




