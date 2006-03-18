-- | Implements the continuation monad transformer.
-- Adds the ability to capture and jump to continuations.

module Monad.ContT 
  ( ContT, runCont, shift, reset, module Monad.Prelude
    -- * Examples
    -- $Examples
  ) where

import Monad.Prelude

-- | Computations that may refer to their continuation,
-- execute in prompt of type /o/,
-- may side-effect as described by /m/,
-- and return a value of type /a/.
newtype ContT o m a = C ((a -> m o) -> m o)

-- | Execute a computation.  We use the identity ('return') continuation.
runCont            :: Monad m => ContT a m a -> m a
runCont (C m)       = m return

-- | Delimits the scope of the captured continuations (introduces a new prompt).
-- This implementation is from Wadler's \"Composable continuations\".
shift         :: Monad m => ((a -> ContT o m o) -> ContT o m o) -> ContT o m a
shift f             = C (\k -> runCont (f (\a -> lift (k a))))

-- | Jumps to a captured continuation
-- This implementation is from Wadler's \"Composable continuations\".
reset              :: Monad m => ContT a m a -> ContT o m a
reset m             = lift (runCont m)


instance Functor (ContT o m) where
  fmap f (C m)      = C (\k -> m (\a -> k (f a)))

instance Monad (ContT o m) where
  return x          = C (\k -> k x)
  C m1 >>= k        = C (\k1 -> m1 (\a -> let C m2 = k a in m2 k1))
  C m >> C n        = C (\k -> m (\_ -> n k))

instance Trans (ContT o) where
  lift m            = C (\k -> k =<< m) 

instance BaseM m b => BaseM (ContT o m) b where
  inBase m          = lift (inBase m)

instance ReaderM m r => ReaderM (ContT o m) r where
  getR              = lift getR

instance WriterM m w => WriterM (ContT o m) w where
  put o             = lift (put o)

instance StateM m s => StateM (ContT o m) s where
  get               = lift get
  set s             = lift (set s)
  update f          = lift (update f)

instance ExceptM m x => ExceptM (ContT o m) x where
  raise x           = lift (raise x)

instance MonadPlus m => MonadPlus (ContT o m) where
  mzero               = lift mzero
  mplus (C m1) (C m2) = C (\k -> m1 k `mplus` m2 k)

instance ContM (ContT o m) where
  callcc f          = C (\k -> let C m = f (\a -> C (\_ -> k a)) in m k)


{- $Examples

Jumping to a continuation does not affect the output.

> prop_ContT'WriterM = test == ["World", "Hello"]
>   where test = runId $ execWriter $ runCont
>              $ do (stop,k) <- returnCC False
>                   if stop then put ["World"]
>                           else do put ["Hello"]
>                                   cJump True k


Jumping to a continuation does not affect the state.

> prop_ContT'StateM = test == (17,17)
>   where test = runId $ runState 42 $ runCont
>              $ do (stop,k) <- returnCC False
>                   if stop then get
>                           else do set 17
>                                   cJump True k



Jumping to a continuation does not cancel backtracking.

> prop_ContT'MonadPlus = test == [42,17]
>   where test = runId $ runSearchAll $ runCont
>              $ do (stop,k) <- returnCC False
>                   if stop then return 42
>                           else cJump True k `mplus` return 17
>


-}

