module Unstable.Control.Monad.Writer (Writer, runWriter, module T)where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.WriterT as W
import Unstable.Control.Monad.Trans as T

type Writer w       = W.WriterT w Identity 

runWriter           :: Writer w a -> (a,w)
runWriter m         = runIdentity (W.runWriter m)


