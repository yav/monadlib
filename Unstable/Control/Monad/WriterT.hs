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
-- The implementation of the writer transformer.  For details of the methods
-- used to manipulate output see the 'MonadWriter' class.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Unstable.Control.Monad.WriterT (

        -- * Type and instances
        WriterT,
        -- ** MonadWriter instance note
        -- $listen

        -- ** MonadNondet instance note
        -- $findAll

        -- * Functions        
        -- ** Removing the transformer
	runWriter, evalWriter, execWriter, 

        -- ** Backward compatibility
        runWriterT, execWriterT, mapWriterT,

	module T,
	module Monoid

          ) where

import Data.Monoid as Monoid (Monoid(..))
import Control.Monad(liftM,MonadPlus(..))
import Control.Monad.Fix

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils


newtype WriterT w m a = W { unW :: m (a, w) }

instance (Monoid w) => MonadTrans (WriterT w) where
  lift m        = W (liftM (\a -> (a,mempty)) m)

instance (Monoid w, HasBaseMonad m n) => HasBaseMonad (WriterT w m) n where
  inBase        = inBase'
  mapBase f     = mapBase' f

instance Monoid w => MapTrans (WriterT w) where
  mapTrans f m  = W (f (unW m))

instance (Monoid w, Monad m) => Functor (WriterT w m) where
  fmap          = liftM

instance (Monoid w, Monad m) => Monad (WriterT w m) where
  return        = return'
  m >>= f       = W (do (a, w)  <- unW m
		        (b, w') <- unW (f a)
		        return (b, w `mappend` w'))
  fail          = fail'

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
  mfix m  = W (mfix (\ ~(a, _) -> unW (m a)))


--------------------------------------------------------------------------------

-- | Remove the writer transformer.  
-- The collected output is returned in the second component of the result tuple. 
runWriter       :: WriterT w m a -> m (a,w)
runWriter       = unW

-- | Similar to 'runWriter' except that the result of the computation is ignored.
-- Convenient for computations, where only the side effect is interesting.
execWriter      :: Monad m => WriterT w m a -> m w 
execWriter m    = liftM snd (unW m)


-- | Similar to 'runWriter' except that the output is ignored.
evalWriter      :: Monad m => WriterT w m a -> m a
evalWriter m    = liftM fst (unW m)



-- | Same as 'runWriter'.
runWriterT      :: WriterT w m a -> m (a,w)
runWriterT      = runWriter

-- | Same as 'execWriter'
execWriterT     :: Monad m => WriterT w m a -> m w
execWriterT     = execWriterT

-- NOTE: Should this be exported?
mapWriterT      :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m  = W (f (unW m))


instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m) where
  ask           = ask'
  local         = local' mapWriterT 

-- $listen
-- Compatibility note: this implementation of the method 'listen' does 
-- not produce any output.  The old behavior of listen is implemented
-- by the function 'listenTell'.


instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell w        = W (return ((), w))
  listen        = mapWriterT (liftM (\(a,w) -> ((a,w),mempty))) 

instance (Monoid w, MonadState s m) => MonadState s (WriterT w m) where
  get           = get'
  put           = put'

instance (Monoid w, MonadError e m) => MonadError e (WriterT w m) where
  raise         = raise'
  handle        = handle1' W unW

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
  mzero         = mzero'
  mplus         = mplus1' W unW



{- $findAll
    'findAll' does not produce output, and simply ignores the output
    of the different alternatives.
    If interested in the output of the alternatives, use 'findAllW' instead.  -}

instance (Monoid w, MonadNondet m) => MonadNondet (WriterT w m) where
  findAll       = mapWriterT (liftM (\xs -> (fmap fst xs, mempty)) . findAll) 

  commit        = mapWriterT commit


-- TODO: can we add a function that will give acess to the output as well,
-- e.g. something like findAllW above.

-- ???
-- in this version the outputs of the "differetn threads" should be separate
-- when combined the other way around (resumptions after output) the threads
-- share a common channel
instance (Monoid w, MonadResume m) => MonadResume (WriterT w m) where
  delay             = mapWriterT delay 
  step v d (W m)    = W (step (unW . v . fst) (unW . d . W) m)
                     




-- jumping undoes the output
instance (Monoid w, MonadCont m) => MonadCont (WriterT w m) where
  callCC        = callCC1' W unW (\a -> (a,mempty)) 


