module Unstable.Control.Monad.State (State, runState, runStateS, module T) where

import Unstable.Control.Monad.Identity  
import qualified Unstable.Control.Monad.StateT as S
import Unstable.Control.Monad.Trans as T

type State s  = S.StateT s Identity

runState      :: s -> State s a -> a
runState s m  = runIdentity (S.runState s m)

runStateS     :: s -> State s a -> (a,s)
runStateS s m = runIdentity (S.runStateS s m)

