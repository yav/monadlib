-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ContT
-- Copyright   :  (c) The University of Glasgow 2001
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-parameter type classes)
--
-- The implementation of the continuation monad transformer.
-- For description of the methods to use continuations see the 'MonadCont' class.
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.ContT (
        -- * Type and instanecs
        ContT, 
        -- | The interaction with other transformers is unfinished.

        -- * Functions
        -- ** Removing the transformer
	runCont, runWithCont,

        -- ** Backward compatability
        runContT, mapContT, withContT,

	module T
  ) where

import Control.Monad(liftM,MonadPlus(..))
import Control.Monad.Fix

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils




newtype ContT r m a   = C { unC :: (a -> m r) -> m r }

instance MonadTrans (ContT r) where
  lift m      = C (m >>=)

instance HasBaseMonad m n => HasBaseMonad (ContT r m) n where
  inBase          = inBase'
  mapBase f (C m) = C (mapBase f . m)


instance (Monad m) => Functor (ContT r m) where
  fmap        = liftM

instance (Monad m) => Monad (ContT r m) where
  return      = return'
  m >>= k     = C (\c -> m $$ (\a -> k a $$ c))


--------------------------------------------------------------------------------

-- | Remove the continuation transformer.  
runCont       :: Monad m => ContT r m r -> m r
runCont       = runWithCont return

-- | Remove the continuation transformer, by specifying explicitly
-- the final continuation.
runWithCont       :: (a -> m b) -> ContT b m a -> m b
runWithCont k m   = unC m k

-- | Like 'runWithCont', but with the argiments the other way around.
-- For backward compatability.
runContT      :: ContT r m a -> (a -> m r) -> m r
runContT      = flip runWithCont

mapContT      :: (m r -> m r) -> ContT r m a -> ContT r m a
mapContT f m  = C (f . (m $$)) 

withContT     :: ((b -> m r) -> (a -> m r)) -> ContT r m a -> ContT r m b
withContT f m = C ((m $$) . f)


-- private:
($$)          :: ContT r m a -> (a -> m r) -> m r
($$)          = unC
--------------------------------------------------------------------------------

instance (MonadReader r' m) => MonadReader r' (ContT r m) where
  ask         = ask'

-- NOTE: the laws do not hold, if 'm' decides to jump, i.e.
-- ignore the continuation where the environment is restored.
  local f m   = C (\k -> do r <- ask 
                            local f (m $$ (\a -> localSet r (k a))))


instance (MonadWriter w m, MonadFix m) => MonadWriter w (ContT r m) where
  tell        = tell'

{-
  listen m    = C (\k -> mdo (r,w1) <- listen (m $$ k')
                             let k' a = do (r,w2) <- listen (k (a,w1))
                                           return r
                                 w2 = w2
                             tell w2
                             return r
                  ) -}
  listen m    = error "listen: continuations after writer not implemenetd (yet?)"
  -- IMPORTANT: This method currently is not implemented.


                        
instance (MonadState s m) => MonadState s (ContT r m) where
  get         = get'
  put         = put'

instance (MonadError e m) => MonadError e (ContT r m) where
  raise       = raise'  
  handle      = handle2' C ($$)

instance MonadPlus m => MonadPlus (ContT r m) where
  mzero       = mzero
  mplus       = mplus2' C ($$)

instance (Monad m) => MonadCont (ContT r m) where
  callCC f    = C (\c -> f (\a -> C (\_ -> c a)) $$ c)



