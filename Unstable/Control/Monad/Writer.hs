-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) OGI at OHSU, 2003
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (multi-param classes, functional dependencies)
--
-- The implementation of the writer monad.  
--
-----------------------------------------------------------------------------


module Unstable.Control.Monad.Writer 
  (Writer, runWriter, execWriter, evalWriter, module T) where

import Unstable.Control.Monad.Identity 
import qualified Unstable.Control.Monad.WriterT as W
import Unstable.Control.Monad.Trans as T

type Writer w       = W.WriterT w Identity 

runWriter           :: Writer w a -> (a,w)
runWriter m         = runIdentity (W.runWriter m)

execWriter          :: Writer w a -> w
execWriter m        = runIdentity (W.execWriter m)

evalWriter          :: Writer w a -> a
evalWriter m        = runIdentity (W.evalWriter m)


