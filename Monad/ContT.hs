-- | Implements the continuation monad transformer.
-- Adds the ability to capture and jump to continuations.
--
-- * Commutes with: "Monad.ContT" (?)
--
-- * Does not commute with: "Monad.ReaderT", "Monad.WriterT", "Monad.StateT", "Monad.ExceptT", "Monad.BackT" 
module Monad.ContT 
  ( -- * Instance notes

    -- ** instance ReaderM
    -- $ReaderM

    -- ** instance WriterM
    -- $WriterM

    -- ** instance StateM
    -- $StateM

    -- ** instance ExceptM
    -- $ExceptM

    -- ** instance BackM
    -- $BackM

  ContT, runCont, shift, reset, module Monad.Prelude
  ) where

import Monad.Prelude
import Control.Monad.Fix

-- | Computations that return a value of type /a/, in prompt of type /o/
-- and in the process can side-effect as described by /m/.
newtype ContT o m a = C ((a -> m o) -> m o)

-- | Execute a computation.  We use the identity (return) continuation.
runCont            :: Monad m => ContT a m a -> m a
runCont (C m)       = m return

-- | Delimits the scope of the captured continuations (introduces a new prompt).
-- This implementation is from Wadler's \"Composable continuations\".
shift              :: Monad m => ((a -> ContT o m o) -> ContT o m o) -> ContT o m a
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

instance Trans (ContT o) where
  lift m            = C (\k -> k =<< m) 

instance BaseM m b => BaseM (ContT o m) b where
  inBase m          = lift (inBase m)

-- $ReaderM
-- Jumping does not affect context, i.e. the context at the target 
-- of a jump would be the same as the context at the source of the jump.
-- This is similar to dynamic scoping.
--
-- see: runReaderIn in <Examples/Cont.hs>
instance ReaderM m r => ReaderM (ContT o m) r where
  get               = lift get
  local f (C m)     = C (\k -> local f (m k))

-- $WriterM
-- Jumping does not affect the output, that is every time we go past
-- a part of the program that has output, we add this output to the buffer.
--
-- see: runWriterIn in <Examples/Cont.hs>
instance WriterM m w => WriterM (ContT o m) w where
  put o             = lift (put o)

-- $StateM
-- Jumping does not affect the state, that is every time we execute a part
-- of the program that modifies the state, it will modified.
-- 
-- see: runStateIn in <Examples/Cont.hs>
instance StateM m s => StateM (ContT o m) s where
  peek              = lift peek
  poke s            = lift (poke s)

-- $ExceptM
-- TODO: What is going on here?
instance ExceptM m x => ExceptM (ContT o m) x where
  raise x           = lift (raise x)
  handle (C m) h    = C (\k -> handle (m k) (\x -> let C m' = h x in m' k))

-- $BackM
-- TODO: What is going on here?
instance MonadPlus m => MonadPlus (ContT o m) where
  mzero                 = lift mzero
  mplus (C m1) (C m2)   = C (\k -> m1 k `mplus` m2 k)

instance ContM (ContT o m) where
  callcc f          = C (\k -> let C m = f (\a -> C (\_ -> k a)) in m k)



