-- | An implementation of the continuation monad.
module Monad.Cont (Cont, runCont, shift, reset, module Monad.Prelude) where

import Monad.Prelude


-- | A computation that produces values of type /a/, in a prompt /o/.
newtype Cont o a  = C ((a -> o) -> o)

-- | Execute a computation. 
runCont          :: Cont a a -> a
runCont (C m)     = m id

-- | Implementation from Wadler's \"Composable continuations\".
shift            :: ((a -> Cont o o) -> Cont o o) -> Cont o a
shift f           = C (\k -> let C m = f (\a -> return (k a))
                             in m id)

-- | Implementation from Wadler's \"Composable continuations\".
reset            :: Cont a a -> Cont o a
reset m           = return (runCont m)

 
instance Functor (Cont o) where
  fmap f (C m)    = C (\k -> m (\a -> k (f a)))

instance Monad (Cont o) where
  return x        = C (\k -> k x)
  C m1 >>= k      = C (\k1 -> m1 (\a -> let C m2 = k a
                                        in m2 k1))
instance BaseM (Cont o) (Cont o) where inBase x = x

instance ContM (Cont o) where
  callcc f        = C (\k -> let C m = f (\a -> C (\_ -> k a)) in m k)


                 









