-- | Implements the state monad.  
module Monad.State (State, runState, evalState, execState, module Monad.Prelude) where

import Monad.Prelude
import Control.Monad.Fix


-- | A computation returns a result of type /a/, and can manipulate 
-- state of type /s/.
newtype State s a = S (s -> (a,s))


-- | Execute a stateful computation, as a result we get
-- the result of the computation, and the final state.
runState         :: s -> State s a -> (a,s)
runState s (S m)  = m s

-- | Execute a stateful computation, ignoring the final state.
evalState        :: s -> State s a -> a
evalState s m     = fst (runState s m)

-- | Execute a stateful computation, just for the side effect.
execState        :: s -> State s a -> s
execState s m     = snd (runState s m)


instance Functor (State s) where
  fmap f (S m)    = S (\s -> let (a,s1) = m s
                             in (f a, s1))

instance Monad (State s) where
  return a        = S (\s -> (a,s))
  S m >>= k       = S (\s -> let (a,s1) = m s 
                                 S n    = k a
                             in n s1)

instance BaseM (State s) (State s) where inBase x = x

instance MonadFix (State s) where
  mfix f          = S (\s -> let S m    = f (fst r)
                                 r      = m s 
                             in r)

instance StateM (State s) s where
  peek            = S (\s -> (s,s))
  poke s          = S (\s1 -> (s1,s))




