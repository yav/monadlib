-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the state monad.  
--
-----------------------------------------------------------------------------

module Unstable.Control.Monad.State (State, runState, evalState, execState, module T) where

import Unstable.Control.Monad.Identity 
import qualified Unstable.Control.Monad.StateT as S
import Unstable.Control.Monad.Trans as T

type State s        = S.StateT s Identity

runState            :: s -> State s a -> (a,s)
runState s m        = runIdentity (S.runState s m)

evalState           :: s -> State s a -> a
evalState s m       = runIdentity (S.evalState s m)

execState           :: s -> State s a -> s
execState s m       = runIdentity (S.execState s m)

