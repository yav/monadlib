module Monad.State 
  ( State
  , run, eval, exec
  , peek, poke, update
  )
  where

import Control.Monad.Fix


newtype State s a = S (s -> (a,s))

instance Functor (State s) where
  fmap f (S m)    = S (\s -> let (a,s1) = m s
                             in (f a, s1))

instance Monad (State s) where
  return a        = S (\s -> (a,s))
  S m >>= k       = S (\s -> let (a,s1) = m s 
                                 S n    = k a
                             in n s1)

instance MonadFix (State s) where
  mfix f          = S (\s -> let S m    = f (fst r)
                                 r      = m s 
                             in r)

-- | Execute a stateful computation.
run              :: s -> State s a -> (a,s)
run s (S m)       = m s

-- | Execute a stateful computation, using local state.
eval             :: s -> State s a -> a
eval s m          = fst (run s m)

-- | Execute a staeful computation, just for the side effect.
exec             :: s -> State s a -> s
exec s m          = snd (run s m)

-- | Get the state.
peek             :: State s s
peek              = S (\s -> (s,s))

-- | Set the state to a particular value.
-- | Returns the old state as a result.
poke             :: s -> State s s
poke s            = S (\s1 -> (s1,s))

-- | Apply a function to the state.
-- | Returns the old state as a result.
update           :: (s -> s) -> State s s
update f          = S (\s -> (s, f s))




