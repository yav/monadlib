-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Cont
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- Continuation monads.
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.ContT (
	ContT,
        runCont,
        runContT,
	mapContT,
	withContT,
	module T
  ) where

import Prelude (Functor(..),Monad(..),(.),error)
import Control.Monad(liftM,MonadPlus(..))

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils


-- unfinished
newtype ContT r m a   = C { unC :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
  lift m      = C (m >>=)

instance HasBaseMonad m n => HasBaseMonad (ContT r m) n where
  inBase      = inBase'

instance (Monad m) => Functor (ContT r m) where
  fmap        = liftM

instance (Monad m) => Monad (ContT r m) where
  return      = return'
  m >>= k     = C (\c -> m $$ (\a -> k a $$ c))


runCont       :: Monad m => ContT r m r -> m r
runCont  m    = m $$ return

runContT      :: ContT r m a -> (a -> m r) -> m r
runContT      = ($$)

mapContT      :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m  = C (f . (m $$)) 

withContT     :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = C ((m $$) . f)

($$)          :: ContT r m a -> (a -> m r) -> m r
($$)          = unC


instance (MonadReader r' m) => MonadReader r' (ContT r m) where
  ask         = ask'
  local f m   = C (\k -> do r <- ask 
                            local f (m $$ (\a -> localSet r (k a))))


instance (MonadWriter w m) => MonadWriter w (ContT r m) where
  tell        = tell'
  listen      = error "listen: continuations after writer not implemenetd (yet?)"
                        
instance (MonadState s m) => MonadState s (ContT r m) where
  get         = get'
  put         = put'

instance (MonadError e m) => MonadError e (ContT r m) where
  throwError  = throwError'  
  catchError  = catchError2' C ($$)

instance MonadPlus m => MonadPlus (ContT r m) where
  mzero       = mzero
  mplus       = mplus2' C ($$)

instance (Monad m) => MonadCont (ContT r m) where
  callCC f    = C (\c -> f (\a -> C (\_ -> c a)) $$ c)



