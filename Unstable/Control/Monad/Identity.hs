-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the identity monad.  
-- 
-- Abstraction for wrapping up an object.  If you have an monadic function, say:
--
-- > example :: Int -> Identity Int
-- > example x = return (x*x)
--
-- you can "run" it, using
--
-- > Main> runIdentity (example 42)
-- > 1764 :: Int
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.Identity (Identity, runIdentity) where

import Prelude(Functor(..),Monad(..),(.))
import Monad(liftM)
import Control.Monad.Fix



newtype Identity a    = I { unI :: a }

instance Functor Identity where
  fmap      = liftM

instance Monad Identity where
  return    = I
  m >>= k   = k (unI m)

instance MonadFix Identity where
  mfix f    = return (fix (runIdentity . f))


-- | Execute a computation in the identity monad.
-- This doesn't do much more than changing the types of things.
runIdentity :: Identity a -> a  
runIdentity = unI



