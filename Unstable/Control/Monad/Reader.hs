-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the reader monad.  
--
-----------------------------------------------------------------------------


module Unstable.Control.Monad.Reader (module T, Reader, runReader, R.local') where

import Unstable.Control.Monad.Identity 
import qualified Unstable.Control.Monad.ReaderT as R
import Unstable.Control.Monad.Trans as T     

type Reader r     = R.ReaderT r Identity

runReader         :: r -> Reader r a -> a
runReader r m     = runIdentity (R.runReader r m)

{-
newtype Reader r a  = R (R.ReaderT r Identity a) 
                    deriving (Functor,Monad,MonadFix,MonadReader r)

instance HasBaseMonad (Reader r) (Reader r) where
  inBase            = id

runReader         :: r -> Reader r a -> a
runReader r (R m) = runIdentity (R.runReader r m)
-}




