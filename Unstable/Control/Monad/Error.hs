-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the error monad.  
--
-----------------------------------------------------------------------------


module Unstable.Control.Monad.Error (Error, runError, module T) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.ErrorT as E
import Unstable.Control.Monad.Trans as T

type Error e  = E.ErrorT e Identity

runError      :: Error e a -> Either e a
runError m    = runIdentity (E.runError m)


