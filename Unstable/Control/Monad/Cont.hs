-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the continuation monad.  
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.Cont (Cont,runCont,runWithCont,module T) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.ContT as C
import Unstable.Control.Monad.Trans as T

type Cont r   = C.ContT r Identity

runCont       :: Cont a a -> a
runCont m     = runIdentity (C.runCont m)

runWithCont     :: (a -> b) -> Cont b a -> b
runWithCont f m = runIdentity (C.runWithCont (\x -> return (f x)) m)


