module Unstable.Control.Monad.Cont (Cont,runCont,module T) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.ContT as C
import Unstable.Control.Monad.Trans as T

type Cont r   = C.ContT r Identity

runCont       :: Cont a a -> a
runCont m     = runIdentity (C.runCont m)


