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
-- The implementation of the reader monad transformer.  For description
-- of the methods to manipulate the environment see the 'MonadReader' class.
--
--	  Inspired by the paper
--	  /Functional Programming with Overloading and
--	      Higher-Order Polymorphism/, 
--	    Mark P Jones (<http://www.cse.ogi.edu/~mpj/>)
--		  Advanced School of Functional Programming, 1995.
-----------------------------------------------------------------------------

module Unstable.Control.Monad.ReaderT (
        -- * Type and instances
	ReaderT, 
        -- * Functions
        -- ** Removing the transformer
        runReader, local', 
        -- ** Backward compatibility
        runReaderT, mapReaderT, withReaderT,
	module T
	) where

import Control.Monad (MonadPlus(..),liftM)
import Control.Monad.Fix

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils hiding (local')


newtype ReaderT r m a = R { unR :: r -> m a }

-- ---------------------------------------------------------------------------
-- Basic instances

instance MonadTrans (ReaderT r) where 
  lift m          = R (const m)

instance HasBaseMonad m n => HasBaseMonad (ReaderT r m) n where
  inBase          = inBase'
  mapBase f       = mapBase f

instance MapTrans (ReaderT r) where
  mapTrans f m    = R (f . unR m)

instance Monad m => Functor (ReaderT r m) where
  fmap            = liftM

instance Monad m => Monad (ReaderT r m) where
  fail            = fail'
  return          = return' 
  m >>= f         = R (\r -> unR m r >>= (\a -> unR (f a) r))

instance MonadFix m => MonadFix (ReaderT r m) where
  mfix f          = R (\r -> mfix (\a -> unR (f a) r))


--------------------------------------------------------------------------------

-- | Remove a reader layer by providing a specific value for the environment.
runReader         :: r -> ReaderT r m a -> m a
runReader r m     = unR m r

-- | A more general version of 'local' that allows the type of the
-- environment to be temporarily changed. 

local'            :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
local' f m        = R (unR m . f)

-- | Same as 'runReader' but with the arguments the other way around.
runReaderT        :: ReaderT r m a -> r -> m a
runReaderT        = unR

-- | Same as 'local''.
withReaderT       :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT       = local' 

-- | Apply a function to underlying monad.  
-- NOTE: Should this be exported?
mapReaderT        :: (m a -> n b) -> ReaderT w m a -> ReaderT w n b
mapReaderT f m    = R (f . unR m)



-- how the features are implemented for readers

instance (Monad m) => MonadReader r (ReaderT r m) where
  ask             = R return
  local           = local'

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
  tell            = tell'
  listen          = listen2' R unR (\w a -> (a,w))

instance MonadState s m => MonadState s (ReaderT r m) where
  get             = get'
  put             = put'

instance MonadError e m => MonadError e (ReaderT r m) where
  raise           = raise'
  handle          = handle2' R unR

instance MonadPlus m => MonadPlus (ReaderT r m) where
  mzero           = mzero'
  mplus           = mplus2' R unR

instance (MonadNondet m) => MonadNondet (ReaderT r m) where
  findAll         = mapReaderT findAll
  commit          = mapReaderT commit 

instance MonadResume m => MonadResume (ReaderT r m) where
  delay           = mapReaderT delay
  step v d (R m)  = R (\e -> let v' = runReader e . v 
                                 d' = runReader e . d . R . const
                             in step v' d' (m e))

instance MonadCont m => MonadCont (ReaderT r m) where
  callCC          = callCC2' R unR const 


