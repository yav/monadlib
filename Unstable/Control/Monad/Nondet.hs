-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the nondeterminism and backtracking monad.  
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.Nondet (
  Nondet, 
  runNondet, runNondets, 
  module T, N.MonadPlus(..)
  ) where

import Unstable.Control.Monad.Identity 
import qualified Unstable.Control.Monad.NondetT as N
import Unstable.Control.Monad.Trans as T

-- this is simply list
type Nondet   = N.NondetT Identity

runNondet     :: Nondet a -> Maybe a
runNondet m   = runIdentity (N.runNondet m)

runNondets    :: Nondet a -> [a]
runNondets m  = runIdentity (N.runNondets m)


