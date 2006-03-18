-- | Implements the writer (aka output) transformer.
-- This is useful when a computation needs to collect a number of items,
-- besides its main result.  This implementation uses a last-in-first-out
-- policiy, so that the last 'put' will appear at the front of the output.

module Monad.WriterT 
  ( WriterT, runWriter, evalWriter, execWriter, module Monad.Prelude
    -- * Examples
    -- $Examples
  ) where

import Monad.StateT
import Monad.Prelude
import Control.Monad.Fix
import Data.Monoid

-- | A computation may place items in a buffer of type /w/, 
-- can side-effect as described by /m/,
-- and computes a result of type /a/, 
newtype WriterT w m a = W { unW :: StateT w m a }

-- | Execute a computation, returning the final result and the output.
runWriter          :: (Monoid w, Monad m) => WriterT w m a -> m (a,w) 
runWriter (W m)     = runState mempty m

-- | Execute a computation ignoring the output.
evalWriter         :: (Monoid w, Monad m) => WriterT w m a -> m a
evalWriter (W m)    = evalState mempty m

-- | Execute a computation just for the output.
execWriter         :: (Monoid w, Monad m) => WriterT w m a -> m w
execWriter (W m)    = execState mempty m



instance Monad m => Functor (WriterT w m) where
  fmap f (W m)      = W (fmap f m)

instance Monad m => Monad (WriterT w m) where
  return a          = W (return a)
  W m >>= f         = W (m >>= (unW . f))
  W m >> W n        = W (m >> n)
                        
instance Trans (WriterT w) where
  lift m            = W (lift m)

instance BaseM m b => BaseM (WriterT w m) b where
  inBase m          = W (inBase m)

instance MonadFix m => MonadFix (WriterT w m) where
  mfix f            = W (mfix (unW . f))
                        
instance ReaderM m r => ReaderM (WriterT w m) r where
  getR              = W getR

instance ReadUpdM m r => ReadUpdM (WriterT w m) r where
  updateR f (W m)   = W (updateR f m)
  setR x (W m)      = W (setR x m)

instance (Monoid w, Monad m) => WriterM (WriterT w m) w where
  put w             = W (update_ (mappend w)) 

instance (Monoid w, Monad m) => CollectorM (WriterT w m) w where
  collect (W m)     = W (do w1 <- set mempty
                            a  <- m 
                            w2 <- set w1
                            return (a,w2))
                          
instance StateM m s => StateM (WriterT w m) s where
  get               = lift get
  set s             = lift (set s)
  update f          = lift (update f)

instance ExceptM m e => ExceptM (WriterT w m) e where
  raise e           = W (raise e)

instance HandlerM m e => HandlerM (WriterT w m) e where
  handle (W m) h    = W (m `handle` (unW . h))

instance MonadPlus m => MonadPlus (WriterT w m) where
  mzero             = W mzero
  mplus (W m) (W n) = W (mplus m n)

instance ContM m => ContM (WriterT w m) where
  callcc m          = W (callcc m')
    where m' k      = unW (m (W . k))
                   

{- $Examples

When we 'put' the new output is to the left of previous output.

> prop_WriterT'WriterM = test == ["World", "Hello"]
>   where test  = runId $ execWriter
>               $ do put ["Hello"]
>                    put ["World"]


Raising an exception undoes the output.

> prop_WriterT'HandlerM = test == Right (42,[])
>   where test  = runId $ runExcept $ runWriter
>               $ do put ["Hello"]
>                    raise "Error"
>                  `handle_` return 42


Backtracking undoes the output.

> prop_WriterT'MonadPlus = test == Just ["World"]
>   where test  = runId $ runSearchOne $ execWriter
>               $ do put ["Hello"]
>                    mzero
>                  `mplus` put ["World"]


Jumping to a continuation undoes changes the output.

> prop_WriterT'ContM = test == ["World"]
>   where test = runId $ runCont $ execWriter
>              $ do (stop,k) <- returnCC False
>                   if stop then put ["World"]
>                           else do put ["Hello"]
>                                   cJump True k

-}                              



