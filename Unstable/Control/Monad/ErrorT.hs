-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.ErrorT
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- This module contains the implementation of the error monad transformer.
-- For a description of the methods for raising and handling errors see
-- the 'MonadError' class.
--
-----------------------------------------------------------------------------



module Unstable.Control.Monad.ErrorT (
        -- * Type and instances
	ErrorT, 
        -- ** MonadPlus instance note
        -- $MonadPlus

        -- ** MonadNondet instance note
        -- $findAll

        -- * Functions
        -- ** Removing the transformer
        runError,
        -- ** Backward compatibility
        runErrorT, mapErrorT,

	module T
  ) where

import Control.Monad(MonadPlus(..),liftM)
import Control.Monad.Fix

import Unstable.Control.Monad.Trans as T
import Unstable.Control.Monad.Private.Utils

newtype ErrorT e m a  = E { unE :: m (Either e a) }

instance MonadTrans (ErrorT e) where
  lift m    = E (liftM Right m)

instance HasBaseMonad m n => HasBaseMonad (ErrorT e m) n where
  inBase    = inBase'
  mapBase f = mapBase' f

instance MapTrans (ErrorT e) where
  mapTrans f m = E (f (unE m))

instance (Monad m) => Functor (ErrorT e m) where
  fmap      = liftM

instance (Monad m) => Monad (ErrorT e m) where
  return    = return'
  m >>= k   = E (do a <- unE m
                    case a of
                      Left  l -> return (Left l)
                      Right r -> unE (k r))
  fail      = fail'   -- use 'throwError' to throw errors.

instance MonadFix m => MonadFix (ErrorT e m) where
  mfix f  = E (mfix (unE . f . either (error "ErrorT: mfix looped") id))


--------------------------------------------------------------------------------

-- | Remove the error transformer.  The resulting computation
-- returns a value of sum type. Errors are reported in the
-- left component of the sum, while results are reported on the right.
runError    :: ErrorT e m a -> m (Either e a)
runError    = unE


-- | Same as 'runError'.
-- For backward compatibility.
runErrorT   :: ErrorT e m a -> m (Either e a)
runErrorT   = runError

-- | Should be private?
mapErrorT   :: (m (Either e a) -> n (Either e' b)) -> ErrorT e m a -> ErrorT e' n b
mapErrorT f = E . f . unE

--------------------------------------------------------------------------------

instance (MonadReader r m) => MonadReader r (ErrorT e m) where
  ask       = ask'
  local     = local' mapErrorT

instance (MonadWriter w m) => MonadWriter w (ErrorT e m) where
  tell      = tell'
  listen    = listen1' E unE (\w -> either Left (\r -> Right (r,w)))

instance (MonadState s m) => MonadState s (ErrorT e m) where
  get       = get'
  put       = put'

instance (Monad m) => MonadError e (ErrorT e m) where
  raise       = E . return . Left 
  handle m h  = E (do a <- unE m
                      case a of
                        Left  l -> unE (h l)
                        Right r -> return (Right r))

-- $MonadPlus
-- Note that we chose to use the 'MonadPlus' class for nondeterminism, 
-- and /not/ error handling.  Thus, the implementations of 'mplus' and 'mzero'
-- do not handle and raise errors, but instead indicate a lack of answer,
-- or different alternatives.   This differs from the instances for
-- the 'Maybe' class from the Prelude, which is rather unfortunate.
instance MonadPlus m => MonadPlus (ErrorT e m) where
  mzero       = mzero'
  mplus       = mplus1' E unE


-- $findAll
-- The method 'findAll' for errors added after nondeterminism behaves a lot like 
-- 'catMaybes' from the "Maybe" module.  It never fails,  but will always
-- return (a possibly empty) list of the answers for the computations that succeeded.
-- If all results (including failed ones) are required, one can use 'findAllE' instead.

instance MonadNondet m => MonadNondet (ErrorT e m) where
  findAll     = mapErrorT (liftM res . findAll)
    where res xs = Right [ x | Right x <- xs ]


  commit      = mapErrorT commit

instance MonadResume m => MonadResume (ErrorT e m) where
  delay           = mapErrorT delay
  step v d (E m)  = E (step v' d' m)
    where v' (Left e)   = return (Left e)
          v' (Right a)  = unE (v a)
          d' m          = unE (d (E m))



instance (MonadCont m) => MonadCont (ErrorT e m) where
  callCC            = callCC1' E unE Right


