-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Andy Gill 2001,
--		  (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the state monad transformer.  For description of
-- the methods used to manipulate state see the 'MonadState' class.
--
--	  This module is inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.StateT (
        -- * Type and instances
	StateT, 
        -- ** MonadNondet instance note
        -- $forAll

        -- * Functions
        -- ** Removing the transformer
        runState, execState, evalState,

        -- ** Backward compatibility
        runStateT, evalStateT, execStateT, mapStateT, withStateT,
	module T

  ) where

import Control.Monad(liftM,MonadPlus(..))
import Control.Monad.Fix

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils


newtype StateT s m a  = S { unS :: s -> m (a,s) }

instance MonadTrans (StateT s) where
  lift m    = S (\s -> liftM (\a -> (a,s)) m)

instance HasBaseMonad m n => HasBaseMonad (StateT s m) n where
  inBase    = inBase'
  mapBase f = mapBase' f

instance MapTrans (StateT s) where
  mapTrans f m = S (f . unS m)
                        

instance (Monad m) => Functor (StateT s m) where
  fmap      = liftM

instance (Monad m) => Monad (StateT s m) where
  return    = return'
  m >>= k   = S (\s -> do (a, s') <- unS m s
		          unS (k a) s')
  fail      = fail'

instance (MonadFix m) => MonadFix (StateT s m) where
  mfix f  = S (\s -> mfix (\ ~(a, _) -> unS (f a) s))

--------------------------------------------------------------------------------

-- | Remove the state monad transformer.  
-- The underlying monad computes a tuple, containing
-- an answer in its first argument, and the final state of the computation in the second.
runState      :: s -> StateT s m a -> m (a,s)
runState s m  = unS m s

-- | Remove the state monad transformer, ignoring the final state.
-- Convenient when the state is only used internally in the computation.
evalState     :: Monad m => s -> StateT s m a -> m a
evalState s m = liftM fst (runState s m)

-- | Remove the state monad transformer, ignoring the final result.
-- Convenient when we are only interested in the side effect, i.e. the state.
execState     :: Monad m => s -> StateT s m a -> m s
execState s m = liftM snd (runState s m)



-- | Similar to 'runState', but with the arguments swapped.
runStateT     ::(Monad m) => StateT s m a -> s -> m (a,s)
runStateT     = flip runState

-- | Similar to 'evalState', but with the arguments swapped.
evalStateT    :: (Monad m) => StateT s m a -> s -> m a
evalStateT    = flip evalState

-- | Similar to 'execState', but with the arguments swapped.
execStateT    :: (Monad m) => StateT s m a -> s -> m s
execStateT    = flip execState

-- | NOTE: What is this for?  
-- Seems to be the same as: @modify f >> m@
withStateT      :: Monad m => (s -> s) -> StateT s m a -> StateT s m a
withStateT f m  = S (unS m . f)

-- | NOTE: Should not be exported?
mapStateT     :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f m = S (f . unS m)

--------------------------------------------------------------------------------


instance (MonadReader r m) => MonadReader r (StateT s m) where
  ask         = ask'
  local       = local' mapStateT

instance (MonadWriter w m) => MonadWriter w (StateT s m) where
  tell        = tell'
  listen      = listen2' S unS (\w (a,s) -> ((a,w),s)) 

instance (Monad m) => MonadState s (StateT s m) where
  get         = S (\s -> return (s, s))
  put s       = S (\_ -> return ((), s))

instance (MonadError e m) => MonadError e (StateT s m) where
  raise       = raise'
  handle      = handle2' S unS

instance (MonadPlus m) => MonadPlus (StateT s m) where
  mzero       = mzero'
  mplus       = mplus2' S unS

instance MonadNondet m => MonadNondet (StateT s m) where

  findAll m   = S (\s -> liftM (\xs -> (fmap fst xs,s)) (findAll (unS m s)))
{- $forAll 
  This implementation of 'findAll' does not affect the state.
  It also does not return the state of the different alternatives, 
  so in a way it is similar to 'runState'.  
  If the state at the end of each alternative is needed, one can use 'findAllS'.
-}

  commit      = mapStateT commit



instance MonadResume m => MonadResume (StateT s m) where

  -- When a delayed computation is activated,
  -- it resumes with the state at the time of the delay.
  delay           = mapStateT delay


  step v d m      = S (\s -> let v' (a,s')  = runState s (v a)   -- or s'?
                                 d' n       = runState s (d (S (const n)))
                             in step v' d' (runState s m))


-- State out
-- s -> Mu X. m (Either (a,s) X)
-- Give me an initial state, and then it may take a few steps 
-- before we compute the next state.  Paused computations are stateless.


-- State in
-- Mu X. s -> m (Either (a,s) X)
-- Paused computations are stateful.


-- jumping undoes changes to the state state
instance MonadCont m => MonadCont (StateT s m) where
  callCC      = callCC2' S unS (\a s -> (a,s))


